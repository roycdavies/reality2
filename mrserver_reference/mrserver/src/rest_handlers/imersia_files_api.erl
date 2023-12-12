% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: File storage API for uploading files
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_files_api).

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_multipart/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2,
    options/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"multipart">>, <<"form-data">>, '*'}, from_multipart}], Req, State}.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Preflight CORS parameters
% ----------------------------------------------------------------------------------------------------
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS, POST, DELETE">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"developerid, location, sessionid, channelid, geobotid, userid, filename, touch">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"$*">>, Req2),
    {ok, Req3, State}.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
	Connection = imersia_db:connect(),
	SessionID = cowboy_req:header(<<"sessionid">>, Req),
	ID = imersia_misc:id_type(cowboy_req:header(<<"channelid">>, Req), cowboy_req:header(<<"geobotid">>, Req), cowboy_req:header(<<"userid">>, Req)),
    Filename = cowboy_req:header(<<"filename">>, Req),

	UserID = get_userid(Connection, SessionID),

    % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{userid => UserID, sessionid => SessionID, id => ID, connection => Connection, filename => Filename, contextids => ContextIDs},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
    Location = cowboy_req:header(<<"location">>, Req2),
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} ->
            Allowance = check_authorization(Connection, cowboy_req:method(Req), ID, UserID, ContextIDs),
            {Parameters2, Response} = case Allowance of
                {ok, Action} ->
                    { Parameters#{action => Action}, true };
                {error, Reason} ->
                    imersia_db:close(Connection),
                    { Parameters, {false, atom_to_list(Reason)} }
            end,
            Req3 = Req2#{parameters => Parameters2},
            {Response, Req3, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {{false, atom_to_list(Reason)} , Req2, State}
    end.

check_authorization(_, <<"OPTIONS">>, _, _, _) -> {ok, options};

check_authorization(Connection, <<"POST">>, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, <<"PUT">>, ChannelID, UserID, ContextIDs);
check_authorization(Connection, <<"DELETE">>, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, <<"PUT">>, ChannelID, UserID, ContextIDs);
check_authorization(Connection, Method, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, Method, ChannelID, UserID, ContextIDs);

check_authorization(Connection, <<"POST">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, <<"PUT">>, GeobotID, undefined, UserID, ContextIDs);
check_authorization(Connection, <<"DELETE">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, <<"PUT">>, GeobotID, undefined, UserID, ContextIDs);
check_authorization(Connection, Method, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, Method, GeobotID, undefined, UserID, ContextIDs);

check_authorization(Connection, Method, {userid, UserID}, UserID, _) ->
    imersia_auth:check_user(Connection, Method, UserID);

check_authorization(_, _, _, _, _) -> {error, id}.

% check_authorization(Connection, <<"DELETE">>, {channelid, ChannelID}, UserID) -> imersia_auth:check_channel(Connection, <<"DELETE">>, ChannelID, UserID, []);
% check_authorization(Connection, Method, {channelid, ChannelID}, UserID) -> imersia_auth:check_geobot(Connection, Method, undefined, ChannelID, UserID, []);
% check_authorization(Connection, Method, {geobotid, GeobotID}, UserID) -> imersia_auth:check_geobot(Connection, Method, GeobotID, undefined, UserID, []);
% check_authorization(_, _, {userid, UserID}, UserID) -> true;
% check_authorization(_, _, _, _) -> {false, <<"Unauthorized">>}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the item ID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) -> do_resource_exists(cowboy_req:method(Req), Req, State).

do_resource_exists(<<"DELETE">>, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{id := {_, GeneralID}, connection := Connection, filename := Filename}} = Req,
    case imersia_files:file_exists(GeneralID, Filename) of
        {ok, exists} ->
            {true, Req, State};
        {error, file} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(filename, Req, State)
    end;

do_resource_exists(_, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{userid := UserID, connection := Connection, id := ID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    Method = cowboy_req:method(Req),
    if
        (UserID == undefined) and (Method /= <<"GET">>) ->
            imersia_db:close(Connection),
            imersia_misc:response_error(sessionid, Req, State);
        true ->
			case check_existance(Connection, ID, UserID) of
                {ok, exists} ->
                    {true, Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end.

check_existance(Connection, {channelid, ChannelID}, _) -> imersia_db:channel_exists(Connection, ChannelID);
check_existance(Connection, {geobotid, GeobotID}, _) -> imersia_db:geobot_exists(Connection, GeobotID);
check_existance(_, {userid, UserID}, UserID) -> {ok, exists};
check_existance(_, {userid, _}, _) -> {error, id};
check_existance(_, _, _) -> {error, id}.
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
% Reads a single file from the multipart data stream and uploads it to the appropriate place in the
% file structure
% TODO: Make it work also for a single file
% ----------------------------------------------------------------------------------------------------
from_multipart(Req, State) -> get_all_files_from_body(Req, State, []).

get_all_files_from_body(Req, State, Filenames) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, id := {_, GeneralID}}} = Req,

    % Get the contents of the body
    case cowboy_req:read_part(Req) of
       {ok, Headers, Req2} ->
            % Get the filename
            case cow_multipart:form_data(Headers) of
                {file, _, Filename, ContentType} ->
                    % Stream the contents of the file
                    {ok, FileData, Req3} = stream_file(Req2, <<>>),
                	case imersia_files:upload(GeneralID, Filename, ContentType, FileData) of
                		{ok, FileURL} ->
                            get_all_files_from_body(Req3, State, [FileURL | Filenames]);
                		{error, Reason} ->
                            get_all_files_from_body(Req3, State, [Reason | Filenames])
                	end;
                _ ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(inputfile, Req2, State)
            end;
        {done, Req4} -> {true, cowboy_req:set_resp_body(jsx:encode([{fileurls, Filenames}]), Req4), State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET - GET produces a list of files, but to get an individual file, use
% /files/ID/filename
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    #{parameters := #{id := {_, GeneralID}, connection := Connection}} = Req,
    case imersia_files:list_all_files(GeneralID) of
        {ok, Filenames} -> imersia_db:close(Connection), {jsx:encode([{fileurls, Filenames}]), Req, State};
        {error, id} -> imersia_db:close(Connection), {jsx:encode([{fileurls, []}]), Req, State}; % No files uploaded yet
        {error, Reason} -> imersia_db:close(Connection), {jsx:encode([{error, Reason}]), Req, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Read binary file data from the body of the message
% ----------------------------------------------------------------------------------------------------
stream_file(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} ->
            stream_file(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} ->
            {ok, << Acc/binary, Data/binary >>, Req}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a File
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, filename := Filename, id := {_, GeneralID}}} = Req,

    case imersia_files:delete_file(GeneralID, Filename) of
        {ok, deleted} ->
            FileURL = << "/files/", GeneralID/binary, "/", Filename/binary >>,
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{fileurl, FileURL}]), Req), State};
        {error, file} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(filename, Req, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
