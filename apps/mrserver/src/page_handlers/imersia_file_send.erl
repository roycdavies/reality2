% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Send File from a Channel, Geobot or Companion
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_file_send).
-include_lib("kernel/include/file.hrl").

-export([init/2]).

% ----------------------------------------------------------------------------------------------------
% Pass the file referred to via the bindings through to the client, processed as specified
% ----------------------------------------------------------------------------------------------------
init(Req, State) ->
	Connection = imersia_db:connect(),

	% Extract the parts of the file details from the URI
	Folder = cowboy_req:binding(folder, Req),
	Filename = cowboy_req:binding(filename, Req),
	Request = json_or_key_value(imersia_misc:safely_decode_json(cow_qs:urldecode(cowboy_req:qs(Req))), cowboy_req:parse_qs(Req)),

	Size = ej:get({"size"}, Request),
	Shape = ej:get({"shape"}, Request),
	URL = ej:get({"url"}, Request),
	Format = ej:get({"format"}, Request),
	SessionIDReq = ej:get({"sessionid"}, Request),
	ContextIDsReq = ej:get({"contextids"}, Request),

	SessionID = select(SessionIDReq, cowboy_req:header(<<"sessionid">>, Req)),
	UserID = get_userid(Connection, SessionID),

	% ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
	ContextIDsRaw = select(ContextIDsReq, cowboy_req:header(<<"contextids">>, Req)),
	ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    Allowance = check_authorization(Connection, cowboy_req:method(Req), Folder, UserID, ContextIDs),
    case Allowance of
	{ok, _} ->
        send_file(Req, State, Folder, Filename, Size, Shape, URL, Format);
    {error, _} ->
        imersia_db:close(Connection),
        {ok, cowboy_req:reply(404, Req), State}

    end.
check_authorization(Connection, <<"GET">>, Folder, undefined, ContextIDs) ->
	check_authorization(Connection, <<"GET">>, Folder, public, ContextIDs);
check_authorization(Connection, <<"GET">>, Folder, UserID, ContextIDs) ->
	case (Folder == UserID) of
		true -> {ok, get};
		_ ->
			case imersia_db:channel_exists(Connection, Folder) of
				{ok, exists} ->
					imersia_auth:check_channel(Connection, <<"GET">>, Folder, UserID, ContextIDs);
				_ ->
					case imersia_db:geobot_exists(Connection, Folder) of
						{ok, exists} ->
							imersia_auth:check_geobot(Connection, <<"GET">>, Folder, undefined, UserID, ContextIDs);
						_ -> {error, id}
					end
			end
	end.

% Choose between JSON or key/Value pairs
json_or_key_value ([{}], Text) -> Text;
json_or_key_value (Json, _) -> Json.

select(undefined, ID2) -> ID2;
select(ID1, undefined) -> ID1;
select(_ID1, ID2) -> ID2.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the UserID from the SessionID
% ----------------------------------------------------------------------------------------------------
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Send the given file to the client
% Makes the assumption that if Size and Shape are defined, then the file is an image for processing
% and processes it, otherwise, just sends it through unchanged.
% ----------------------------------------------------------------------------------------------------
send_file(Req, State, Folder, Filename, undefined, undefined, undefined, undefined) ->
	LocalFilename = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(Folder) ++ "/" ++ erlang:binary_to_list(Filename),

	% Send a blank image if the file referenced doesn't exist
	case file:read_file_info(LocalFilename) of
		{ok, FileData} ->
			% Get the MIME type
			Mimetype = mimerl:filename(list_to_binary(LocalFilename)),
			% Get the File Size
			FileSize = FileData#file_info.size,
			{ok, cowboy_req:reply(200, #{ <<"content-type">> => Mimetype}, {sendfile, 0, FileSize, LocalFilename}, Req), State};
		_ ->
			{ok, cowboy_req:reply(404, Req), State}
	end;

send_file(Req, State, undefined, undefined, Size, Shape, undefined, Format) ->
	case imersia_files:downloadurl(undefined) of
		{ok, Mimetype, FileData} ->
			ProcessedFile = process_file(FileData, Size, Shape, Format),
			{ok, cowboy_req:reply(200, #{<<"content-type">> => Mimetype, <<"accept-ranges">> => <<"bytes">>}, ProcessedFile, Req), State};
		{error, undefined, reading} ->
			{ok, cowboy_req:reply(404, Req), State}
	end;
send_file(Req, State, Folder, Filename, Size, Shape, undefined, Format) ->
	case imersia_files:download(Folder, Filename) of
		{ok, Mimetype, FileData} ->
			ProcessedFile = process_file(FileData, Size, Shape, Format),
			{ok, cowboy_req:reply(200, #{<<"content-type">> => Mimetype, <<"accept-ranges">> => <<"bytes">>}, ProcessedFile, Req), State};
		{error, undefined, reading} ->
			{ok, cowboy_req:reply(404, Req), State}
	end;
send_file(Req, State, _, _, Size, Shape, URL, Format) ->
	case imersia_files:downloadurl(URL) of
		{ok, Mimetype, FileData} ->
			ProcessedFile = process_file(FileData, Size, Shape, Format),
			{ok, cowboy_req:reply(200, #{<<"content-type">> => Mimetype, <<"accept-ranges">> => <<"bytes">>}, ProcessedFile, Req), State};
		{error, undefined, reading} ->
			{ok, cowboy_req:reply(404, Req), State}
	end.

process_file(FileData, undefined, undefined, undefined) -> FileData;
process_file(FileData, Size, Shape, Format) ->
		Pixels = case Size of
			<<"thumb">> -> 64;
			<<"small">> -> 256;
			<<"medium">> -> 512;
			_ -> 1024
		end,
		FormatAtom = case Format of
			<<"jpg">> -> jpg;
			<<"png">> -> png;
			_ -> png
		end,
		imersia_files:process_image(FileData, Pixels, Shape, FormatAtom).
% ----------------------------------------------------------------------------------------------------
