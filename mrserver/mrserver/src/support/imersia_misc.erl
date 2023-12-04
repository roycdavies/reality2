% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Various miscellaneous functions
% ----------------------------------------------------------------------------------------------------

-module(imersia_misc).
-include("../imersia_datatypes.hrl").
-include("../qrcode/qrcode.hrl").

-export([
	basedir/0,
	to_list/1,
	safely_decode_json/1,
	safely_decode_json/2,
	safely_encode_json/1,
	safely_convert_to_map/1,
	safely_convert_from_map/1,
	record_to_json/2,
	json_to_record/2,
	unify_location/1,
	unify_location/4,
	convert_arrays_for_rethinkdb/1,
	make_binary/1,
	id_type/3,
	redirect_page/3,
	response_error/3,
	response_error_true/3,
	meld_records/2,
	interpret_body/2,
	distance/4,
	is_json_term/1,
	convert_number/1,
	convert_number/2,
	convert_class/1,
	convert_bool/1,
	convert_bool/2,
	add_cors/1,
	add_cors/2,
	add_cors/3,
	simple_png_encode/1,
	debug/3,
	debug_to_logfile/4
]).


% ----------------------------------------------------------------------------------------------------
% Return the main physical directory where everything is placed.
% ----------------------------------------------------------------------------------------------------
basedir() ->
    {ok, Dir} = file:get_cwd(),
    [ReturnDir | _] = string:split(Dir, "/mrserver"),
    ReturnDir.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert whatever Value comes in to a list / string
% ----------------------------------------------------------------------------------------------------
to_list(Value) when is_integer (Value) ->
	erlang:integer_to_list(Value);

to_list(Value) when is_binary (Value) ->
	erlang:binary_to_list(Value);

to_list(Value) when is_atom (Value) ->
	erlang:atom_to_list(Value);

to_list(Value) when is_float (Value) ->
	erlang:float_to_list(Value).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Some useful functions to safely encode and decode JSON
% ----------------------------------------------------------------------------------------------------
safely_decode_json(JsonToDecode) -> safely_decode_json(JsonToDecode, return_empty).
safely_decode_json(JsonToDecode, return_maps) ->
	try jsx:decode(JsonToDecode, []) of
		Result -> Result
	catch
		_:_ -> [{}]
	end;
safely_decode_json(JsonToDecode, return_empty) ->
    try jsx:decode(JsonToDecode, [{return_maps, false}]) of
        Result -> Result
    catch
        _:_ -> [{}]
    end;
safely_decode_json(JsonToDecode, _) ->
    try jsx:decode(JsonToDecode, [{return_maps, false}]) of
        Result -> Result
    catch
        _:_ -> JsonToDecode
    end.

safely_encode_json(JsonToEncode) ->
    try jsx:encode(JsonToEncode) of
        Result -> Result
    catch
        _:_ -> <<"{}">>
    end.
safely_convert_to_map(JSON) ->
	JSONText = imersia_misc:safely_encode_json(JSON),
	imersia_misc:safely_decode_json(JSONText, return_maps).
safely_convert_from_map(JSONMap) ->
	JSONText = imersia_misc:safely_encode_json(JSONMap),
	imersia_misc:safely_decode_json(JSONText).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
add_cors(Req) ->
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).
add_cors(Methods, Req) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req),
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1).
add_cors(Methods, Headers, Req) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req1),
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert records to JSON for storage or other use.  If you don't want a particular field in the
% resulting JSON, set it's value in the record to null, and it will be removed.
% TODO: use is_record instead of relying on tuple representation of records.
% ----------------------------------------------------------------------------------------------------
record_to_json(null, _) -> null;
record_to_json(undefined, _) -> undefined;
record_to_json(RecordDetails, RemoveNulls) ->
	RecordName = element(1, RecordDetails),
	Result = case RecordName of
		user ->
			RecordDetails2 = RecordDetails#user{
				details = record_to_json(RecordDetails#user.details, RemoveNulls)
			},
			RecordDetails3 = RecordDetails2#user{
				location = record_to_json(RecordDetails2#user.location, RemoveNulls)
			},
			lists:zip(record_info(fields, user), tl(tuple_to_list(RecordDetails3)));
		user_details -> lists:zip(record_info(fields, user_details), tl(tuple_to_list(RecordDetails)));
		session -> lists:zip(record_info(fields, session), tl(tuple_to_list(RecordDetails)));
    	location -> lists:zip(record_info(fields, location), tl(tuple_to_list(RecordDetails)));
		channel -> lists:zip(record_info(fields, channel), tl(tuple_to_list(RecordDetails)));
		geobot ->
			RecordDetails2 = RecordDetails#geobot{
				location = record_to_json(RecordDetails#geobot.location, RemoveNulls)
			},
			lists:zip(record_info(fields, geobot), tl(tuple_to_list(RecordDetails2)));
		geobotlite ->
			RecordDetails2 = RecordDetails#geobotlite{location = record_to_json(RecordDetails#geobotlite.location, RemoveNulls)},
			lists:zip(record_info(fields, geobotlite), tl(tuple_to_list(RecordDetails2)));
		metadata ->
			RecordDetails2 = RecordDetails#metadata{value = convert_value_to_json(RecordDetails#metadata.value)},
			lists:zip(record_info(fields, metadata), tl(tuple_to_list(RecordDetails2)));
		analytic ->
			RecordDetails2 = RecordDetails#analytic{
				params = convert_value_to_json(RecordDetails#analytic.params),
				indexes = convert_value_to_json(RecordDetails#analytic.indexes),
				location = record_to_json(RecordDetails#analytic.location, RemoveNulls)
			},
			lists:zip(record_info(fields, analytic), tl(tuple_to_list(RecordDetails2)));
		action ->
			RecordDetails2 = RecordDetails#action{
				parameters = convert_value_to_json(RecordDetails#action.parameters)
			},
			lists:zip(record_info(fields, action), tl(tuple_to_list(RecordDetails2)));
		transition ->
			RecordDetails2 = RecordDetails#transition{
				actions = convert_actions_to_json(RecordDetails#transition.actions, RemoveNulls)
			},
			lists:zip(record_info(fields, transition), tl(tuple_to_list(RecordDetails2)));
		automation ->
			RecordDetails2 = RecordDetails#automation{
				transitions = convert_transitions_to_json(RecordDetails#automation.transitions, RemoveNulls)
			},
			lists:zip(record_info(fields, automation), tl(tuple_to_list(RecordDetails2)));
		context ->
			lists:zip(record_info(fields, context), tl(tuple_to_list(RecordDetails)));
		passcode ->
			lists:zip(record_info(fields, passcode), tl(tuple_to_list(RecordDetails)))
	end,
	remove_nulls(Result, RemoveNulls).

remove_nulls([], _) -> [];
remove_nulls([{_, null}|Tail], true) -> remove_nulls(Tail, true);
remove_nulls([{_, undefined}|Tail], true) -> remove_nulls(Tail, true);
remove_nulls([{Key, Value}|Tail], true) -> [{Key, Value}] ++ remove_nulls(Tail, true);
remove_nulls([{Key, null}|Tail], false) -> [{Key, <<"">>}] ++ remove_nulls(Tail, false);
remove_nulls([{Key, undefined}|Tail], false) -> [{Key, <<"">>}] ++ remove_nulls(Tail, false);
remove_nulls([{Key, Value}|Tail], false) -> [{Key, Value}] ++ remove_nulls(Tail, false).

convert_value_to_json(Value) ->
    case imersia_misc:safely_decode_json(Value) of
        [{}] -> Value;
        JSON -> JSON
    end.

convert_transitions_to_json([], _) -> [];
convert_transitions_to_json([Transition | Transitions], RemoveNulls) ->
	[record_to_json(Transition, RemoveNulls) | convert_transitions_to_json(Transitions, RemoveNulls)].

convert_actions_to_json([], _) -> [];
convert_actions_to_json([Action | Actions], RemoveNulls) ->
	[record_to_json(Action, RemoveNulls) | convert_actions_to_json(Actions, RemoveNulls)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a JSON representation of a Record to the specified Record
% ----------------------------------------------------------------------------------------------------
json_to_record(Type, Details) ->
	try json_to_record2(Type, Details) of
		Result -> Result
	catch
		_:_ -> error
	end.

json_to_record2(channel, Details) ->
	#channel{
		channelid = nullify(ej:get({"id"}, Details)),
		ownerid = nullify(ej:get({"ownerid"}, Details)),
		name = nullify(ej:get({"name"}, Details)),
		description = nullify(ej:get({"description"}, Details)),
		class = nullify(ej:get({"class"}, Details)),
		imageurl = nullify(ej:get({"imageurl"}, Details)),
		hidden = case nullify(ej:get({"hidden"}, Details)) of null -> false; Value -> Value end,
		created = convert_time(ej:get({"created"}, Details)),
		modified = convert_time(ej:get({"modified"}, Details))
	};
json_to_record2(session, Details) ->
	#session{
		sessionid = nullify(ej:get({"id"}, Details)),
		userid = nullify(ej:get({"ownerid"}, Details)),
		token = nullify(ej:get({"token"}, Details)),
		created = convert_time(ej:get({"created"}, Details))
	};
json_to_record2(user, Details) ->
	#user{
		userid = nullify(ej:get({"id"}, Details)),
		useremail = nullify(ej:get({"useremail"}, Details)),
		password = nullify(ej:get({"password"}, Details)),
		location = case nullify(ej:get({"location"}, Details)) of
			null -> null;
			Location -> json_to_record(location, Location)
		end,
		details = #user_details{
			firstname = nullify(ej:get({"details", "firstname"}, Details)),
			surname = nullify(ej:get({"details", "surname"}, Details)),
			nickname = nullify(ej:get({"details", "nickname"}, Details)),
			imageurl = nullify(ej:get({"details", "imageurl"}, Details))
		},
		created = convert_time(ej:get({"created"}, Details)),
		modified = convert_time(ej:get({"modified"}, Details))
	};
json_to_record2(user_details, Details) ->
	#user_details{
		firstname = nullify(ej:get({"firstname"}, Details)),
		surname = nullify(ej:get({"surname"}, Details)),
		nickname = nullify(ej:get({"nickname"}, Details)),
		imageurl = nullify(ej:get({"imageurl"}, Details))
	};
json_to_record2(location, Details) ->
	unify_location(
		nullify(ej:get({"latitude"}, Details)),
		nullify(ej:get({"longitude"}, Details)),
		nullify(ej:get({"altitude"}, Details)),
		nullify(ej:get({"geohash"}, Details))
	);
json_to_record2(geobot, Details) ->
	#geobot{
		geobotid = nullify(ej:get({"id"}, Details)),
		channelid = nullify(ej:get({"channelid"}, Details)),
		class = nullify(ej:get({"class"}, Details)),
		ownerid = nullify(ej:get({"ownerid"}, Details)),
		location = case nullify(ej:get({"location"}, Details)) of
			null -> null;
			Location -> json_to_record(location, Location)
		end,
		name = nullify(ej:get({"name"}, Details)),
		description = nullify(ej:get({"description"}, Details)),
		imageurl = nullify(ej:get({"imageurl"}, Details)),
		radius = convert_number(nullify(ej:get({"radius"}, Details))),
		hidden = case nullify(ej:get({"hidden"}, Details)) of null -> false; Value -> Value end,
		created = convert_time(ej:get({"created"}, Details)),
		modified = convert_time(ej:get({"modified"}, Details))
	};
json_to_record2(geobotlite, Details) ->
	#geobotlite{
		geobotid = nullify(ej:get({"id"}, Details)),
		channelid = nullify(ej:get({"channelid"}, Details)),
		ownerid = nullify(ej:get({"ownerid"}, Details)),
		location = case nullify(ej:get({"location"}, Details)) of
			null -> null;
			Location -> json_to_record(location, Location)
		end,
		radius = convert_number(nullify(ej:get({"radius"}, Details)))
	};
json_to_record2(metadata, Details) ->
	#metadata{
		metadataid = nullify(ej:get({"id"}, Details)),
		key = nullify(ej:get({"key"}, Details)),
		value = nullify(ej:get({"value"}, Details))
	};
json_to_record2(analytic, Details) ->
	#analytic{
		analyticid = nullify(ej:get({"id"}, Details)),
		statid = nullify(ej:get({"statid"}, Details)),
		location = case nullify(ej:get({"location"}, Details)) of
			null -> null;
			Location -> json_to_record(location, Location)
		end,
	    created = convert_time(ej:get({"created"}, Details)),
	    event = nullify(ej:get({"event"}, Details)),
	    tally = convert_number(nullify(ej:get({"tally"}, Details))),
	    params = nullify(ej:get({"params"}, Details)),
		indexes = nullify(ej:get({"indexes"}, Details))
	};
json_to_record2(action, Details) ->
	#action{
		command = nullify(ej:get({"command"}, Details)),
		parameters = nullify(ej:get({"parameters"}, Details))
	};
json_to_record2(transition, Details) ->
	#transition{
		state = nullify(ej:get({"state"}, Details)),
		event = nullify(ej:get({"event"}, Details)),
		newstate = nullify(ej:get({"newstate"}, Details)),
		actions = convert_actions_from_json(ej:get({"actions"}, Details))
	};
json_to_record2(automation, Details) ->
	#automation{
		automationid = nullify(ej:get({"id"}, Details)),
		name = nullify(ej:get({"name"}, Details)),
		description = nullify(ej:get({"description"}, Details)),
		commands = nullify(ej:get({"commands"}, Details)),
		transitions = convert_transitions_from_json(ej:get({"transitions"}, Details))
	};
json_to_record2(context, Details) ->
	#context{
		contextid = nullify(ej:get({"id"}, Details)),
		attributes = nullify(ej:get({"attributes"}, Details))
	};
json_to_record2(passcode, Details) ->
	#passcode{
		passcodeid = nullify(ej:get({"id"}, Details)),
		passcode = nullify(ej:get({"passcode"}, Details)),
		useremail = nullify(ej:get({"useremail"}, Details)),
		created = convert_time(ej:get({"created"}, Details)),
		used = convert_bool(ej:get({"used"}, Details)),
		count = convert_number(nullify(ej:get({"count"}, Details)))
	}.

convert_transitions_from_json([]) -> [];
convert_transitions_from_json([Transition | Transitions]) ->
	[json_to_record(transition, Transition) | convert_transitions_from_json(Transitions)].

convert_actions_from_json([]) -> [];
convert_actions_from_json([Action | Actions]) ->
	[json_to_record(action, Action) | convert_actions_from_json(Actions)].

% Convert valid time to ISO8601 formatted time
convert_time(undefined) -> null;
convert_time(null) -> null;
convert_time(Time) -> iso8601:parse(Time).

% Convert a list or binary to a number, but leave a number as a number
convert_number(N) -> convert_number(N, 0).
convert_number(null, Pref) -> Pref;
convert_number(<<>>, Pref) -> Pref;
convert_number("", Pref) -> Pref;
convert_number(undefined, Pref) -> Pref;
convert_number(N, _) when is_number(N) -> N;
convert_number(N, Pref) when is_binary(N) ->
    list_to_number(binary_to_list(N), Pref);
convert_number(N, Pref) when is_list(N) ->
	list_to_number(N, Pref);
convert_number(_, Pref) -> Pref.

list_to_number(L, Pref) ->
    try list_to_float(L)
    catch
        error:badarg ->
            try list_to_integer(L)
			catch
				error:badarg -> Pref
			end
    end.

convert_class(undefined) -> <<"com.imersia.default">>;
convert_class(null) -> <<"com.imersia.default">>;
convert_class(<<>>) -> <<"com.imersia.default">>;
convert_class("") -> <<"com.imersia.default">>;
convert_class(Class) -> Class.

convert_bool(Bool) -> convert_bool(Bool, false).
convert_bool(undefined, Pref) -> Pref;
convert_bool(null, Pref) -> Pref;
convert_bool(<<>>, Pref) -> Pref;
convert_bool("", Pref) -> Pref;
convert_bool(<<"true">>, _) -> true;
convert_bool("true", _) -> true;
convert_bool(<<"false">>, _) -> false;
convert_bool("false", _) -> false;
convert_bool(<<"TRUE">>, _) -> true;
convert_bool("TRUE", _) -> true;
convert_bool(<<"FALSE">>, _) -> false;
convert_bool("FALSE", _) -> false;
convert_bool(-1, _) -> false;
convert_bool(0, _) -> false;
convert_bool(1, _) -> true;
convert_bool(false, _) -> false;
convert_bool(true, _) -> true;
convert_bool(_, Pref) -> Pref.



% Take a location record and make sure GeoHash and Latitudes and Longitude are unified
unify_location(null) -> null;
unify_location(undefined) -> null;
unify_location(Location) when is_record(Location, location) ->
	unify_location(Location#location.latitude, Location#location.longitude, Location#location.altitude, Location#location.geohash);
unify_location(Geohash) -> unify_location(null, null, 0, Geohash).

% Unify Geohash with Latitude and Longitude
unify_location(null, null, null, null) -> null; % Nothing sent, return null
unify_location(null, _, _, null) -> null; % Not enough info to create a valid location
unify_location(_, null, _, null) -> null; % Not enough info to create a valid location
unify_location(_, null, Altitude, Geohash) -> % Geohash sent, but longitude not sent
	[{lat, Latitude}, {lon, Longitude}] = geohash:decode(erlang:binary_to_list(Geohash)),
	#location{latitude=check_latitude(Latitude), longitude=check_longitude(Longitude), altitude=zero_altitude(Altitude), geohash=Geohash};
unify_location(null, _, Altitude, Geohash) -> % Geohash sent, but latitude not sent
	[{lat, Latitude}, {lon, Longitude}] = geohash:decode(erlang:binary_to_list(Geohash)),
	#location{latitude=check_latitude(Latitude), longitude=check_longitude(Longitude), altitude=zero_altitude(Altitude), geohash=Geohash};
unify_location(Latitude, Longitude, Altitude, _) -> % Set the Geohash from the Latitude and Longitude
	Geohash = list_to_binary(geohash:encode(Latitude, Longitude)),
	#location{latitude=check_latitude(Latitude), longitude=check_longitude(Longitude), altitude=convert_number(Altitude), geohash=Geohash}.

% Ensure Altitude is not undefined.
zero_altitude(null) -> 0;
zero_altitude(undefined) -> 0;
zero_altitude(<<>>) -> 0;
zero_altitude("") -> 0;
zero_altitude(Altitude) -> convert_number(Altitude).

% Ensure Latitude is valid values
check_latitude(Latitude) ->
	LatitudeNum = convert_number(Latitude),
	if
		LatitudeNum > 90 ->
			LatitudeNum - 180;
		LatitudeNum < -90 ->
			LatitudeNum + 180;
		true ->
			LatitudeNum
	end.

% Ensure Longitude is valid values
check_longitude(Longitude) ->
	LongitudeNum = convert_number(Longitude),
	if
		LongitudeNum > 180 ->
			LongitudeNum - 360;
		LongitudeNum < -180 ->
			LongitudeNum + 360;
		true ->
			LongitudeNum
	end.

% Make sure undefined is returned as null
nullify(undefined) -> null;
nullify(X) -> X.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Some functions required for mnesia
% ----------------------------------------------------------------------------------------------------
meld_records(Old, New) ->
    list_to_tuple([element(1, Old) | enmesh(tl(tuple_to_list(Old)), tl(tuple_to_list(New)))]).

enmesh([], _) -> [];
enmesh(_, []) -> [];
enmesh([Head1 | Tail1], [Head2 | Tail2]) when is_tuple(Head2) and is_atom(element(1, Head2))->
	[meld_records(Head1, Head2) | enmesh(Tail1, Tail2)];
enmesh([Head1 | Tail1], [Head2 | Tail2]) when (Head2 =:= null) or (Head2 =:= undefined) ->
	[Head1 | enmesh(Tail1, Tail2)];
enmesh([_ | Tail1], [Head2 | Tail2]) ->
	[Head2 | enmesh(Tail1, Tail2)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a string / list into unicode binary, or leave the same if some other type
% ----------------------------------------------------------------------------------------------------
make_binary(X) when is_list(X) ->
    unicode:characters_to_binary(X);
make_binary(X) -> X.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Arrays have to be converted to [2, OriginalArray], otherwise RethinkDB tries to interpret it as a command
% ----------------------------------------------------------------------------------------------------
convert_arrays_for_rethinkdb([]) -> [2, []];
convert_arrays_for_rethinkdb(Element) ->
    process_element(Element).

process_array([]) -> [];
process_array([Head | Tail]) ->
    [process_element(Head) | process_array(Tail)];
process_array(Element) when is_tuple(Element) ->
    process_tuple(Element, {}, 1);
process_array(Element) -> Element.

process_element([]) -> [2, []];
process_element(Element) when is_list(Element) ->
	case is_json_term(Element) of
		true -> process_array(Element);
		false -> [2, process_array(Element)]
	end;
process_element(Element) -> process_array(Element).

process_tuple(Tuple, Sofar, Counter) when Counter =< tuple_size(Tuple) ->
    Element = erlang:element(Counter, Tuple),
    process_tuple(Tuple, erlang:insert_element(tuple_size(Sofar)+1, Sofar, process_element(Element)), Counter + 1);
process_tuple(_, Sofar, _) -> Sofar.

is_json_term([]) -> true;
is_json_term([Head | Tail]) when is_tuple(Head) ->
	is_json_term(Tail);
is_json_term(_) -> false.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% determine the ID type.  Called from the API using this command:
% 	ID = id_type(cowboy_req:header(<<"channelid">>, Req), cowboy_req:header(<<"geobotid">>, Req), cowboy_req:header(<<"userid">>, Req)),
% ----------------------------------------------------------------------------------------------------
id_type(undefined, undefined, undefined) -> {error, undefined};
id_type(ChannelID, undefined, undefined) -> {channelid, ChannelID};
id_type(undefined, GeobotID, undefined) -> {geobotid, GeobotID};
id_type(undefined, undefined, UserID) -> {userid, UserID};
id_type(ChannelID, undefined, _) -> {channelid, ChannelID};
id_type(ChannelID, _, undefined) -> {channelid, ChannelID};
id_type(undefined, GeobotID, _) -> {geobotid, GeobotID};
id_type(_, GeobotID, undefined) -> {geobotid, GeobotID};
id_type(undefined, _, UserID) -> {userid, UserID};
id_type(_, undefined, UserID) -> {userid, UserID};
id_type(_, _, UserID) -> {userid, UserID}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
redirect_page(Req, State, Page) ->
	Req0 = cowboy_req:set_resp_header(<<"Location">>, Page, Req),
	Req1 = cowboy_req:reply(302, Req0),
	{ok, Req1, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
response_error_true(Reason, Req, State) ->
	Req0 = cowboy_req:set_resp_body([<<"{\"error\":\"">>, list_to_binary(atom_to_list(Reason)), <<"\"}">>], Req),
	{true, Req0, State}.
response_error(Reason, Req, State) ->
	Req0 = cowboy_req:set_resp_body([<<"{\"error\":\"">>, list_to_binary(atom_to_list(Reason)), <<"\"}">>], Req),
	{false, Req0, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Grab the body of the message
% ----------------------------------------------------------------------------------------------------
interpret_body(true, Req) ->
	case read_body(Req) of
		{ok, BodyRaw, _} ->
			imersia_misc:safely_decode_json(BodyRaw);
		_ -> {[]}
	end;

interpret_body(false, _) ->
    {[]}.

read_body(Req0) -> read_body(Req0, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Calculate the distance between two geospatial points
% ----------------------------------------------------------------------------------------------------
distance(Lat1, Lng1, Lat2, Lng2) ->
    Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
    [RLng1, RLat1, RLng2, RLat2] = [Deg2rad(Deg) || Deg <- [Lng1, Lat1, Lng2, Lat2]],

    DLon = RLng2 - RLng1,
    DLat = RLat2 - RLat1,

    A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),

    C = 2 * math:asin(math:sqrt(A)),

    %% suppose radius of Earth is 6372.8 km
    M = 6372800 * C,
    M.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Prints out a debug message if debug is turned on in the settings, or an error message
% TODO: Log to a file as well if setting turned on for that.
% ----------------------------------------------------------------------------------------------------
debug_to_logfile(ID, Level, Message, Parameters) when is_binary(Message) ->
	MessageList = erlang:binary_to_list(Message),
	debug_to_logfile(ID, Level, MessageList, Parameters);
debug_to_logfile(ID, Level, Message, Parameters) ->
	Date = iso8601:format(calendar:universal_time()),
	ConsoleMessage = erlang:binary_to_list(ID) ++ " : " ++ Message,
	debug(Level, ConsoleMessage, Parameters),
	case Level of
		error ->
			LogMessage = io_lib:format("ERROR (~s) : " ++ Message, [Date | Parameters]),
			imersia_files:write_to_logfile(ID, LogMessage);
		_ ->
			Connection = imersia_db:connect(),
			case imersia_db:metadata_get(Connection, ID, <<"debug">>) of
				{ok, Metadata} ->
					case Metadata#metadata.value of
						true ->
							LogMessage = io_lib:format("DEBUG (~s) : " ++ Message, [Date | Parameters]),
							imersia_files:write_to_logfile(ID, LogMessage);
						_ -> ok
					end;
				_ -> ok
			end,
			imersia_db:close(Connection),
			ok
	end.

debug(Level, Message, Parameters) ->
	Date = iso8601:format(calendar:universal_time()),
	case Level of
		error ->
			io:format("ERROR (~s) : " ++ Message, [Date | Parameters]), ok;
		_ ->
			case imersia_settings:get_setting(mrserver, debug) of
				<<"true">> -> io:format("DEBUG (~s) : " ++ Message, [Date | Parameters]), ok;
				_ -> ok
			end
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
simple_png_encode(QRCode) ->
	Dim = QRCode#qrcode.dimension,
	Data = QRCode#qrcode.data,
	MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
	Size = Dim * 8,
	IHDR = png_chunk(<<"IHDR">>, <<Size:32, Size:32, 8:8, 2:8, 0:24>>),
	PixelData = get_pixel_data(Dim, Data),
	IDAT = png_chunk(<<"IDAT">>, PixelData),
	IEND = png_chunk(<<"IEND">>, <<>>),
	<<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.

png_chunk(Type, Bin) ->
	Length = byte_size(Bin),
	CRC = erlang:crc32(<<Type/binary, Bin/binary>>),
	<<Length:32, Type/binary, Bin/binary, CRC:32>>.

get_pixel_data(Dim, Data) ->
	Pixels = get_pixels(Data, 0, Dim, <<>>),
	zlib:compress(Pixels).

get_pixels(<<>>, Dim, Dim, Acc) ->
	Acc;
get_pixels(Bin, Count, Dim, Acc) ->
	<<RowBits:Dim/bits, Bits/bits>> = Bin,
	Row = get_pixels0(RowBits, <<0>>), % row filter byte
	FullRow = binary:copy(Row, 8),
	get_pixels(Bits, Count + 1, Dim, <<Acc/binary, FullRow/binary>>).

get_pixels0(<<1:1, Bits/bits>>, Acc) ->
	Black = binary:copy(<<0>>, 24),
	get_pixels0(Bits, <<Acc/binary, Black/binary>>);
get_pixels0(<<0:1, Bits/bits>>, Acc) ->
	White = binary:copy(<<255>>, 24),
	get_pixels0(Bits, <<Acc/binary, White/binary>>);
get_pixels0(<<>>, Acc) ->
	Acc.
% ----------------------------------------------------------------------------------------------------
