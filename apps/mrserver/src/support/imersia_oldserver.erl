-module(imersia_oldserver).
-include("../imersia_datatypes.hrl").

-export([build_user/2, read_analytics/1]).

% ----------------------------------------------------------------------------------------------------
% Public Functions
% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
% Build the user in the database, including all their Channels and Geobots.
% ----------------------------------------------------------------------------------------------------
build_user(EmailAddress, Password) ->
	case get_users_channels_and_geobots(EmailAddress, Password) of
		{ok, UserMap, UserStructure} ->
			create_users_channels_and_geobots(UserMap, UserStructure);
		{error, Error} ->
			{error, Error}
	end.

read_analytics(ID) ->
	io:format("Reading Analytics~n", []),
	case process_analytics_csv_file(imersia_misc:basedir() ++ "/files/oldserver/Analytics.csv", ID) of
		{ok, Analytics} ->
			Analytics;
		{error, Error} -> {error, Error}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Private Functions
% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
% Get the User, Channels and Geobots
% ----------------------------------------------------------------------------------------------------
get_users_channels_and_geobots(EmailAddress, Password) ->
	io:format("Reading Metadata from ~s~n", [imersia_misc:basedir() ++ "/files/oldserver/Metadata.csv"]),
	case process_metadata_csv_file(imersia_misc:basedir() ++ "/files/oldserver/Metadata.csv") of
		{ok, Metadata} ->
			case get_user_by_email(EmailAddress, Password, Metadata) of
				{ok, UserMap} ->
					{UserID, UserDetails, UserMetadata} = UserMap,
					io:format("Reading Channels~n", []),
					case process_csv_file(imersia_misc:basedir() ++ "/files/oldserver/UsersInLayers.csv") of
						{ok, ChannelIDs} ->
							case process_csv_file(imersia_misc:basedir() ++ "/files/oldserver/Layers.csv") of
								{ok, ChannelRecords} ->
									io:format("Reading Geobots~n", []),
									case process_csv_file(imersia_misc:basedir() ++ "/files/oldserver/PointsInLayers.csv") of
										{ok, GeobotIDs} ->
											io:format("Reading Geobots Details~n", []),
											case process_csv_file(imersia_misc:basedir() ++ "/files/oldserver/Points.csv") of
												{ok, GeobotRecords} ->
													io:format("Processing Data~n", []),
													UserChannelIDs = get_channel_ids(UserID, ChannelIDs),
													UserChannels = get_channels(UserChannelIDs, ChannelRecords, GeobotIDs, GeobotRecords, Metadata),
													{ok, {UserDetails, UserMetadata}, UserChannels};
												{error, Error} -> {error, Error}
											end;
										{error, Error} -> {error, Error}
									end;
								{error, Error} -> {error, Error}
							end;
						{error, Error} -> {error, Error}
					end;
				{error, Error} -> {error, Error}
			end;
		{error, Error} -> {error, Error}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the user details from their email address
% ----------------------------------------------------------------------------------------------------
get_user_by_email(EmailAddress, Password, Metadata) ->
	io:format("Reading User Database~n", []),
	case process_csv_file(imersia_misc:basedir() ++ "/files/oldserver/Users.csv") of
		{ok, UserMaps} ->
			case find_user(EmailAddress, Password, UserMaps, Metadata) of
				error -> {error, "User doesnt exist"};
				UserDetails -> {ok, UserDetails}
			end;
		{error, Error} -> {error, Error}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create the user, channels and geobots, from the data given, which has been extracted from the old server
% ----------------------------------------------------------------------------------------------------
create_users_channels_and_geobots(User, UserStructure) ->
	erlang:display("Creating users, channels and geobots"),
	{NewUser, NewUserMetadata} = User,
	Connection = imersia_db:connect(),
	case user_new(Connection, NewUser) of
		{ok, NewUserID} ->
			io:format("User Created :~s~n", [NewUserID]),
			metadata_new(Connection, NewUserID, NewUserMetadata),
			io:format("User Metadata Created~n", []),
			move_files(NewUserID),
			io:format("User Files Moved~n", []),

			create_channels_and_geobots(Connection, NewUserID, UserStructure);
		Error -> erlang:display(Error)
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create the channels with metadata and their associated geobots
% ----------------------------------------------------------------------------------------------------
create_channels_and_geobots(_, _, []) -> ok;
create_channels_and_geobots(Connection, UserID, [Channel | Channels]) ->
	{ChannelDetails, ChannelMetadata, Geobots} = Channel,
	case channel_new(Connection, UserID, ChannelDetails) of
		{ok, ChannelID} ->
			io:format("Channel Created :~s~n", [ChannelID]),
			metadata_new(Connection, ChannelID, ChannelMetadata),
			io:format("Channel Metadata Created~n", []),
			move_files(ChannelID),
			io:format("Channel Files Moved~n", []),

			create_geobots(Connection, ChannelID, Geobots);
		{error, Reason} ->
			io:format("ERROR Creating Channel :~w~n", [Reason])
	end,
	create_channels_and_geobots(Connection, UserID, Channels).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a Geobot with Metadata
% ----------------------------------------------------------------------------------------------------
create_geobots(_, _, []) -> ok;
create_geobots(Connection, ChannelID, [Geobot | Geobots]) ->
	{GeobotDetails, GeobotMetadata} = Geobot,
	case geobot_new(Connection, ChannelID, GeobotDetails) of
		{ok, GeobotID} ->
			io:format("Geobot Created :~s~n", [GeobotID]),
			erlang:display(GeobotMetadata),
			metadata_new(Connection, GeobotID, GeobotMetadata),
			io:format("Geobot Metadata Created~n", []),
			move_files(GeobotID),
			io:format("Geobot Files Moved~n", []);

		{error, Reason} ->
			io:format("ERROR Creating Geobot :~w~n", [Reason])
	end,
	create_geobots(Connection, ChannelID, Geobots).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Process a CSV file and convert it into Maps, with map keys being the first row of the csv file
% ----------------------------------------------------------------------------------------------------
process_csv_file(FileName) ->
    case file:open(FileName,[read]) of
        {ok, FilePID} ->
            RawData = imersia_csv:parse_from_io(FilePID),
			RecordLength = find_record_length(RawData),
			Result = break_into_pieces(RecordLength, RawData),
			ResultMaps = mapify(grab_headers(Result)),
			file:close(FilePID),
			{ok, ResultMaps};
        Error -> {error, Error}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The Metadata values can contain almost anything, and break the csv reading module, so doing it differently
% ----------------------------------------------------------------------------------------------------
process_metadata_csv_file(FileName) ->
	case file:open(FileName,[read]) of
		{ok, FilePID} ->
			Result = read_metadata_line_by_line(FilePID, []),
			ResultMaps = mapify(grab_headers(Result)),
			file:close(FilePID),
			{ok, ResultMaps};
		Error -> {error, Error}
	end.


read_metadata_line_by_line(FilePID, Accum) ->
	case file:read_line(FilePID) of
		{ok, Data} ->
		 	DataNoEOL = string:sub_string(Data, 1, length(Data)-1),
			[split_metadata_line(DataNoEOL) | read_metadata_line_by_line(FilePID, Accum)];
		eof -> Accum;
		{error, _} -> Accum
	end.


split_metadata_line(Data) ->
	[ID, Data1] = string:split(Data, ","),
	[Key, Data2] = string:split(Data1, ","),
	[Hidden, Value] = string:split(Data2, ","),
	[ID, Key, Hidden, string:strip(Value, both, $")].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The Analytics file is so big, it needs special attention
% ----------------------------------------------------------------------------------------------------
process_analytics_csv_file(FileName, ID) ->
	case file:open(FileName,[read]) of
		{ok, FilePID} ->
			Result = read_analytics_line_by_line(FilePID, ID, []),
			% ResultMaps = mapify(grab_headers(Result)),
			file:close(FilePID),
			{ok, Result};
		Error -> {error, Error}
	end.

read_analytics_line_by_line(FilePID, ID, Accum) ->
	IDWithComma = ID ++ ",",
	case file:read_line(FilePID) of
		{ok, Data} ->
			case string:prefix(Data, IDWithComma) of
				nomatch -> read_analytics_line_by_line(FilePID, ID, Accum);
				RestOfData -> [split_analytics_line(RestOfData) | read_analytics_line_by_line(FilePID, ID, Accum)]
			end;
		eof -> Accum;
		{error, _} -> Accum
	end.

split_analytics_line(Data) ->
	Data1 = string:join(string:replace(Data, "\"\n", "", all), ""),
	Data2 = string:join(string:replace(Data1, "\"\"", "\'", all), ""),
	string:join(string:replace(Data2, "\"", ""), "").
% 	[ID, Data1] = string:split(Data, ","),
	% [Key, Data2] = string:split(Data1, ","),
	% [Hidden, Value] = string:split(Data2, ","),
	% [ID, Key, Hidden, string:strip(Value, both, $")].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Find the given user, and return their details and metadata
% ----------------------------------------------------------------------------------------------------
find_user(UserEmail, Password, Records, Metadata) ->
	case do_find_user(UserEmail, Records) of
		{value, UserMap} ->
			case maps:find("Id", UserMap) of
				{ok, UserID} ->
					UserDetails = convert_user_to_record(UserMap, Password),
					UserMetadata = get_metadata(string:lowercase(UserID), Metadata),
					{UserID, UserDetails, UserMetadata};
				_ -> error
			end;
		false -> error
	end.

do_find_user(UserEmail, Records) ->
	lists:search(
		fun (Record) ->
			case maps:find("UserName", Record) of
				{ok, UserEmail} -> true;
				_ -> false
			end
		end,
		Records
	).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get a list of ChannelIDs for the given UserID
% ----------------------------------------------------------------------------------------------------
get_channel_ids(UserID, Records) ->
	LowerCaseUserID = string:lowercase(UserID),
	lists:filtermap(
		fun(Record) ->
			case maps:find("UserId", Record) of
				{ok, UserIDMixedCase} ->
					erlang:display(UserIDMixedCase),
					LowerCaseUserIDFromRecord = string:lowercase(UserIDMixedCase),
					if
						LowerCaseUserIDFromRecord == LowerCaseUserID ->
							#{"LayerId" := ChannelID} = Record,
							{true, string:lowercase(ChannelID)};
						true -> false
					end;
				_ -> false
			end
		end,
		Records
	).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get a list of GeobotIDs for the given ChannelID
% ----------------------------------------------------------------------------------------------------
get_geobot_ids(ChannelID, Records) ->
	LowerCaseChannelID = string:lowercase(ChannelID),
	lists:filtermap(
		fun(Record) ->
			case maps:find("LayerId", Record) of
				{ok, ChannelIDMixedCase} ->
					LowerCaseChannelIDFromRecord = string:lowercase(ChannelIDMixedCase),
					if
						LowerCaseChannelIDFromRecord == LowerCaseChannelID ->
							#{"PointId" := GeobotID} = Record,
							{true, string:lowercase(GeobotID)};
						true -> false
					end;
				_ -> false
			end
		end,
		Records
	).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Channel Details for each of the ChannelIDs
% ----------------------------------------------------------------------------------------------------
get_channels(ChannelIDs, ChannelRecords, GeobotIDs, GeobotRecords, Metadata) ->
	lists:map(
		fun (ChannelID) ->
			find_channel(ChannelID, ChannelRecords, GeobotIDs, GeobotRecords, Metadata)
		end,
		ChannelIDs
	).

find_channel(ChannelID, ChannelRecords, GeobotIDs, GeobotRecords, Metadata) ->
	LowerCaseChannelID = string:lowercase(ChannelID),
	[Result] = lists:filtermap(
		fun(ChannelRecord) ->
			case maps:find("LayerId", ChannelRecord) of
				{ok, ChannelIDMixedCase} ->
					LowerCaseChannelIDFromRecord = string:lowercase(ChannelIDMixedCase),
					if
						LowerCaseChannelIDFromRecord == LowerCaseChannelID ->
							ChannelDetails = convert_channel_to_record(ChannelRecord),
							ChannelMetadata = get_metadata(LowerCaseChannelID, Metadata),
							ChannelGeobotIDs = get_geobot_ids(LowerCaseChannelID, GeobotIDs),
							ChannelGeobots = get_geobots(LowerCaseChannelID, ChannelGeobotIDs, GeobotRecords, Metadata),
							{true, {ChannelDetails, ChannelMetadata, ChannelGeobots}};
						true -> false
					end;
				_ -> false
			end
		end,
		ChannelRecords
	),
	Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Geobots on a Channel
% ----------------------------------------------------------------------------------------------------
get_geobots(ChannelID, GeobotIDs, GeobotRecords, Metadata) ->
	lists:map(
		fun (GeobotID) ->
			find_geobot(ChannelID, GeobotID, GeobotRecords, Metadata)
		end,
		GeobotIDs
	).

find_geobot(ChannelID, GeobotID, GeobotRecords, Metadata) ->
	LowerCaseGeobotID = string:lowercase(GeobotID),
	erlang:display(LowerCaseGeobotID),
	erlang:display(GeobotID),
	Result = lists:filtermap(
		fun(GeobotRecord) ->
			case maps:find("PointId", GeobotRecord) of
				{ok, GeobotIDMixedCase} ->
					LowerCaseGeobotIDFromRecord = string:lowercase(GeobotIDMixedCase),
					erlang:display(LowerCaseGeobotIDFromRecord),
					if
						LowerCaseGeobotIDFromRecord == LowerCaseGeobotID ->
							{GeobotDetails, LeftOvers} = convert_geobot_to_record(ChannelID, GeobotRecord),
							GeobotMetadata = get_metadata(LowerCaseGeobotID, Metadata),
							{true, {GeobotDetails, GeobotMetadata ++ LeftOvers}};
						true -> false
					end;
				_ -> erlang:display(GeobotRecord), false
			end
		end,
		GeobotRecords
	),
	erlang:display(Result),
	[Output] = Result,
	Output.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get all the metadata key value pairs for a given ID
% ----------------------------------------------------------------------------------------------------
get_metadata(ID, Records) ->
	LowerCaseID = string:lowercase(ID),
	lists:filtermap(
		fun(Record) ->
			case maps:find("ID", Record) of
				{ok, MetadataIDMixedCase} ->
					LowerCaseMetadataID = string:lowercase(MetadataIDMixedCase),
					if
						LowerCaseMetadataID == LowerCaseID ->
							{true, convert_metadata_to_record(Record)};
						true -> false
					end;
				_ -> false
			end
		end,
		Records
	).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Move the directory of files into place, if it exists, or return an error otherwise.
% ----------------------------------------------------------------------------------------------------
move_files(ID) ->
	LowerCaseID = string:lowercase(ID),
	ExistingFolderName = imersia_misc:basedir() ++ "/files/oldserver/files/" ++ erlang:binary_to_list(LowerCaseID),
	NewFolderName = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(LowerCaseID),
	case file:rename(ExistingFolderName, NewFolderName) of
		ok -> ok;
		{error, eexist} -> io:format("~s directory already exists~n", [LowerCaseID]);
		{error, enoent} -> io:format("~s directory does not exist~n", [LowerCaseID]);
		{error, Reason} -> io:format("~s error ~w~n", [LowerCaseID, Reason])
	end.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Convert a User Map into a User Record
% ----------------------------------------------------------------------------------------------------
convert_user_to_record(UserMap, Password) ->
	{ok, Salt} = bcrypt:gen_salt(),
	{ok, EncryptedPasswordString} = bcrypt:hashpw(Password, Salt),
	EncryptedPassword = imersia_misc:make_binary(EncryptedPasswordString),

	#user{
		userid = erlang:list_to_binary(string:lowercase(maps:get("Id", UserMap, ""))),
		useremail = erlang:list_to_binary(string:lowercase(maps:get("UserName", UserMap, ""))),
		password = EncryptedPassword,
		details = #user_details{
			firstname = <<"">>,
			surname = <<"">>,
			nickname = <<"">>
		},
		location = imersia_misc:unify_location(null, null, 0, <<"xbpbpbpbpbpbpbp">>),
		created = iso8601:format(calendar:universal_time()),
		modified = iso8601:format(calendar:universal_time())
	}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a Channel Map into a Channel Record
% ----------------------------------------------------------------------------------------------------
convert_channel_to_record(ChannelMap) ->
	#channel{
		channelid = erlang:list_to_binary(string:lowercase(maps:get("LayerId", ChannelMap, ""))),
		ownerid = erlang:list_to_binary(string:lowercase(maps:get("OwnerUserId", ChannelMap, ""))),
		name = erlang:list_to_binary(maps:get("LayerName", ChannelMap, "")),
		description = erlang:list_to_binary(maps:get("Description", ChannelMap, "")),
		class = <<"com.imersia.default">>,
		imageurl = erlang:list_to_binary(parse_channel_imageurl(maps:get("ImageURL", ChannelMap, ""))),
		hidden = case maps:get("Hidden", ChannelMap, false) of "TRUE" -> true; _ -> false end,
		created = convert_value_to_date(maps:get("CreatedDate", ChannelMap, undefined)),
		modified = convert_value_to_date(maps:get("UpdatedDate", ChannelMap, undefined))
	}.

parse_channel_imageurl(ImageURL) ->
	Result1 = string:join(string:replace(ImageURL,"http://files.imersia.net","/files"),""),
	string:join(string:replace(Result1,"https://files.imersia.net","/files"),"").
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a Metadata Map into a Metadata Record
% ----------------------------------------------------------------------------------------------------
convert_metadata_to_record(MetadataMap) ->
	#metadata{
		metadataid = undefined,
		key = erlang:list_to_binary(maps:get("Key", MetadataMap, "")),
		value = convert_value_to_datatype(erlang:list_to_binary(maps:get("Value", MetadataMap, "")))
	}.

convert_value_to_datatype(Value) ->
	case check_conversion (Value, imersia_misc:convert_number(Value, Value)) of
		{converted, Result} -> Result;
		{not_converted} ->
			case check_conversion(Value, imersia_misc:convert_bool(Value, Value)) of
				{converted, Result} -> Result;
				{not_converted} -> Value
			end
	end.

% Date format in the files is Month/Day/Year Hour:Minute where year might be 2 or 4 digit
convert_value_to_date(undefined) -> iso8601:format(calendar:universal_time());
convert_value_to_date(Value) ->
	{{TYear, TMonth, TDay}, {THour, TMinute, _}} = calendar:universal_time(),
	[VMonth, Value2] = string:split(Value, "/"),
	[VDay, Value3] = string:split(Value2, "/"),
	[VYear, Value4] = string:split(Value3, " "),
	[VHour, VMinute] = string:split(Value4, ":"),

	RawYear = imersia_misc:convert_number(VYear, TYear),
	Month = imersia_misc:convert_number(VMonth, TMonth),
	Day = imersia_misc:convert_number(VDay, TDay),
	Year = if RawYear < 100 -> RawYear+2000; true -> RawYear end, % Assumes any YY format are 20YY
	Hour = imersia_misc:convert_number(VHour, THour),
	Minute = imersia_misc:convert_number(VMinute, TMinute),
	iso8601:format({{Year, Month, Day}, {Hour, Minute, 0}}).

check_conversion(Value, Value) -> {not_converted};
check_conversion(_, Result) -> {converted, Result}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a Geobot Map into a Geobot Record
% ----------------------------------------------------------------------------------------------------
convert_geobot_to_record(ChannelID, GeobotMap) ->
	Location = imersia_misc:unify_location(
		imersia_misc:convert_number(maps:get("Latitude", GeobotMap, "")),
		imersia_misc:convert_number(maps:get("Longitude", GeobotMap, "")),
		imersia_misc:convert_number(maps:get("Altitude", GeobotMap, "")),
		null),

	Geobot = #geobot{
		geobotid = erlang:list_to_binary(string:lowercase(maps:get("PointId", GeobotMap, ""))),
	    channelid = erlang:list_to_binary(ChannelID),
	    ownerid = erlang:list_to_binary(string:lowercase(maps:get("CreatorId", GeobotMap, ""))),
	    name = erlang:list_to_binary(maps:get("Title", GeobotMap, "")),
	    description = erlang:list_to_binary(maps:get("Description", GeobotMap, "")),
	    class = <<"com.imersia.default">>,
	    imageurl = erlang:list_to_binary(parse_geobot_imageurl(maps:get("ImageURL", GeobotMap, ""))),
	    location = Location,
	    radius = imersia_misc:convert_number(maps:get("Radius", GeobotMap, 0)),
	    hidden = case maps:get("Hidden", GeobotMap, false) of "TRUE" -> true; _ -> false end,
		created = convert_value_to_date(maps:get("CreatedDate", GeobotMap, undefined)),
		modified = convert_value_to_date(maps:get("ModifiedDate", GeobotMap, undefined))
	},
	LeftOvers = [
		convert_metadata_to_record(#{
			"Key" => "SoundURL",
			"Value" => parse_geobot_soundurl(maps:get("SoundURL", GeobotMap, ""))
		}),
		convert_metadata_to_record(#{
			"Key" => "MoreInfoURL",
			"Value" => maps:get("MoreInfoURL", GeobotMap, "")
		})
	],
	{Geobot, LeftOvers}.

parse_geobot_soundurl(SoundURL) ->
	Result1 = string:join(string:replace(SoundURL,"http://files.imersia.net","/files"),""),
	string:join(string:replace(Result1,"https://files.imersia.net","/files"),"").

parse_geobot_imageurl(ImageURL) ->
	ImageURLBinary = erlang:list_to_binary(ImageURL),
	ListOfURLs = imersia_misc:safely_decode_json(ImageURLBinary),
	FirstImageURL = case grab_first_of_five(ListOfURLs) of
		error -> strip_trailing_and_leading(ImageURL);
		First -> First
	end,
	Result5 = string:join(string:replace(FirstImageURL, "http://files.imersia.net", "/files"), ""),
	string:join(string:replace(Result5, "https://files.imersia.net", "/files"), "").

grab_first_of_five([First, _, _, _, _]) -> erlang:binary_to_list(First);
grab_first_of_five(_) -> error.

strip_trailing_and_leading(ImageURL) ->
	ImageURL1 = string:join(string:replace(ImageURL,"['",""),""),
	string:join(string:replace(ImageURL1,"', '', '', '', '']",""),"").
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The following functions depend on the imersia_csv code, which divides a CSV file into a list with
% records separated by the atom 'newline', and then assumes each record is the same length.
% ----------------------------------------------------------------------------------------------------

% Find the length of the record by looking at the first row
find_record_length(ProcessedCSV) ->
	find_record_length(0, ProcessedCSV).
find_record_length(Counter, []) -> Counter;
find_record_length(Counter, [newline | _] ) -> Counter;
find_record_length(Counter, [_ | RestOfCSV]) -> find_record_length(Counter+1, RestOfCSV).

% Break the list at the newlines into separate lists
break_into_pieces(RecordLength, ProcessedCSV) ->
	do_break_into_pieces(RecordLength, ProcessedCSV, []).

do_break_into_pieces(_, [], Accum) -> Accum;
do_break_into_pieces(RecordLength, [newline | ProcessedCSV], Accum) ->
	[Accum | do_break_into_pieces(RecordLength, ProcessedCSV, [])];
do_break_into_pieces(RecordLength, [Item | ProcessedCSV], Accum) ->
	do_break_into_pieces(RecordLength, ProcessedCSV, [Item | Accum]).

% Get the headers of the file
grab_headers([Headers | List]) ->
	{Headers, List}.

% Using the headers, turn the remaining lines into maps
mapify({_, []}) -> [];
mapify({Headers, [OneRecord | Records]}) ->
	[make_map(Headers, OneRecord) | mapify({Headers, Records})].

make_map([], _) -> #{};
make_map(_, []) -> #{};
make_map([Key | Headers], [Value | List]) ->
	TheMap = make_map(Headers, List),
	TheMap#{Key => Value}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new user in MongoDB with the given user details, including the UserID
% ----------------------------------------------------------------------------------------------------
user_new(Connection, DBEntry) ->
    % Check this user doesn't already exist
	UserEmail = DBEntry#user.useremail,
	NewUserID = DBEntry#user.userid,
    UserID = imersia_db:user_id(Connection, UserEmail),
    case UserID of
        % User doesn't yet exist
        {error, useremail} ->
            DBEntryMap = maps:remove(<<"userid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

            % Create new database for this Companion / User
            case mc_worker_api:connect ([{database, <<"imersia_", NewUserID/binary>>}]) of
                {ok, Connection2} ->
                    mc_worker_api:insert(Connection2, <<"details">>, DBEntryMap#{<<"_id">> => NewUserID}),
                    mc_worker_api:disconnect(Connection2),

                    % Insert entry into the admin database
                    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
                        {ok, Connection3} ->
                            AdminDBEntry = #{
                                <<"_id">> => NewUserID,
                                <<"useremail">> => UserEmail,
                                <<"location">> => #{
                                    <<"type">> => <<"Point">>,
                                    <<"coordinates">> => [180.0, 0.0, 0.0]
                                }
                            },
                            mc_worker_api:insert(Connection3, <<"companions">>, AdminDBEntry),
                            mc_worker_api:disconnect(Connection3),
                            {ok, NewUserID};
                        _ -> {error, database}
                    end;
                _ -> {error, database}
            end;
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            % This user already exists
            {error, useremail}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new metadata entry for the given ID
% ----------------------------------------------------------------------------------------------------
metadata_new(_, _, []) -> ok;
metadata_new(Connection, ID, [Metadata | MetadataList]) ->
	imersia_db:metadata_set(Connection, ID, Metadata#metadata.key, Metadata#metadata.value),
	metadata_new(Connection, ID, MetadataList).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new channel in MongoDB using the details given
% ----------------------------------------------------------------------------------------------------
channel_new(_Connection, UserID, DBEntry) ->
	Name = DBEntry#channel.name,
	ChannelID = DBEntry#channel.channelid,
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"channels">>, #{<<"name">> => Name}) of
                undefined ->
                    % Name doesn't exist so carry on
                    DBEntryMap = maps:remove(<<"channelid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

                    % Create new database for this Channel and set the details
                    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
                        {ok, Connection3} ->
                            mc_worker_api:insert(Connection3, <<"details">>, DBEntryMap#{<<"_id">> => ChannelID}),
                            mc_worker_api:disconnect(Connection3),

                            % Insert entry into the admin database
                            AdminDBEntry = #{
                                <<"_id">> => ChannelID,
                                <<"ownerid">> => UserID,
                                <<"name">> => Name
                            },
                            mc_worker_api:insert(Connection2, <<"channels">>, AdminDBEntry),
                            mc_worker_api:disconnect(Connection2),
                            {ok, ChannelID};

                        _ ->
                            mc_worker_api:disconnect(Connection2),
                            {error, database}
                    end;
                _ ->
                    % Name already exists, so return error
                    mc_worker_api:disconnect(Connection2),
                    {error, name}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new Geobot in MongoDB using the details given
% ----------------------------------------------------------------------------------------------------
geobot_new(_Connection, ChannelID, DBEntry) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
			erlang:display(DBEntry),
			GeobotID = DBEntry#geobot.geobotid,
			UserID = DBEntry#geobot.ownerid,
            DBEntryMap = maps:remove(<<"geobotid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

            % Create new database for this Geobot and set the details
            case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
                {ok, Connection3} ->
                    mc_worker_api:insert(Connection3, <<"details">>, DBEntryMap#{<<"_id">> => GeobotID}),
                    mc_worker_api:disconnect(Connection3),

                    % Insert entry into the admin database
                    AdminDBEntry = #{
                        <<"_id">> => GeobotID,
                        <<"ownerid">> => UserID,
                        <<"channelid">> => ChannelID,
                        <<"location">> => #{
                            <<"type">> => <<"Point">>,
                            <<"coordinates">> => [
                                DBEntry#geobot.location#location.longitude,
                                DBEntry#geobot.location#location.latitude,
                                DBEntry#geobot.location#location.altitude
                            ]
                        }
                    },
                    mc_worker_api:insert(Connection2, <<"geobots">>, AdminDBEntry),
                    mc_worker_api:disconnect(Connection2),
                    {ok, GeobotID};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {error, database}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------
