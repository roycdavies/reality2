% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Metadata Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_metadata_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Collect common parameters
	Connection = imersia_db:connect(),
	SessionID = cowboy_req:header(<<"sessionid">>, Req),
	ID = imersia_misc:id_type(cowboy_req:header(<<"channelid">>, Req), cowboy_req:header(<<"geobotid">>, Req), cowboy_req:header(<<"userid">>, Req)),
    Key = cowboy_req:header(<<"key">>, Req),

    % Get a metadata ID if it exists
    {_, GeneralID} = ID,
    MetadataID = get_metadata_id(Connection, GeneralID, Key, cowboy_req:header(<<"metadataid">>, Req)),

    % And the User's ID from the SessionID
	UserID = get_userid(Connection, SessionID),

    % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, sessionid => SessionID, userid => UserID, id => ID, key => Key, contextids => ContextIDs, metadataid => MetadataID},
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

% Get the UserID from the SessionID
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.

% Get the Metadata ID from the Key, or undefined if it doesn't exist
get_metadata_id(Connection, ID, Key, undefined) ->
    case imersia_db:metadata_id(Connection, ID, Key) of
        {ok, MetadataID} -> MetadataID;
        _ -> undefined
    end;
get_metadata_id(_, _, _, MetadataID) -> MetadataID.

% Get the metadata ID from Metadata Record or the Header, or the Key if both of those are undefined
get_metadata_id_from_record(Connection, ID, Key, undefined, Metadata) ->
    case Metadata#metadata.metadataid of
        undefined -> get_metadata_id(Connection, ID, Key, undefined);
        null -> get_metadata_id(Connection, ID, Key, undefined);
        MetadataID -> MetadataID
    end;
get_metadata_id_from_record(_, _, _, MetadataIDFromHeader, _) ->
    MetadataIDFromHeader.

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the item ID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    case do_check_developerid(Req, State) of
        {true, _, _} -> do_resource_exists(cowboy_req:method(Req), Req, State);
        Error -> Error
    end.

do_resource_exists(<<"HEAD">>, Req, State) ->
    #{parameters := #{connection:= Connection, metadataid := MetadataID, id := {_, GeneralID}}} = Req,
    case MetadataID of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(metadataid, Req, State);
        _ ->
            case imersia_db:metadata_exists(Connection, GeneralID, MetadataID) of
                {ok, exists} ->
                    {true, Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end;

do_resource_exists(<<"DELETE">>, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, metadataid := MetadataID}} = Req,
    case MetadataID of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(metadataid, Req, State);
        _ -> {true, Req, State}
    end;

do_resource_exists(<<"GET">>, Req, State) ->
    #{parameters := #{connection:= Connection, key := Key, metadataid := MetadataID, id := {_, GeneralID}}} = Req,
    case MetadataID of
        undefined -> % Get list of metadata
            if
                (Key =/= undefined) ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(key, Req, State);
                true -> {true, Req, State}
            end;
        _ ->
            case imersia_db:metadata_exists(Connection, GeneralID, MetadataID) of
                {ok, exists} -> {true, Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end;

do_resource_exists(_, Req, State) -> {true, Req, State}.

do_check_developerid(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{userid := UserID, connection := Connection, id := ID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    Method = cowboy_req:method(Req),
    if
        (UserID == undefined) and (Method /= <<"GET">>) -> imersia_misc:response_error(sessionid, Req, State);
        true ->
			case check_existance(Connection, ID, UserID) of
                {ok, exists} -> {true, Req, State};
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
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, id := {_, GeneralID}, metadataid := MetadataID}} = Req,
    % Get the metadata value
    get_metadata(Connection, GeneralID, MetadataID, Req, State).

% No Key or MetadataID defined, so send a list of all Metadata
get_metadata(Connection, GeneralID, undefined, Req, State) ->
    case imersia_db:metadata_list(Connection, GeneralID) of
        {ok, MetadataList} ->
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(convert_metadata_to_json(MetadataList)), Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason}]), Req, State}
    end;
% Valid MetadataID or Key
get_metadata(Connection, GeneralID, MetadataID, Req, State) ->
    case imersia_db:metadata_get_by_id(Connection, GeneralID, MetadataID) of
        {ok, Metadata} ->
            MetadataJSON = imersia_misc:record_to_json(Metadata#metadata{value = imersia_misc:safely_decode_json(Metadata#metadata.value, return_value)}, true),
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(MetadataJSON), Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason}]), Req, State}
    end.

convert_metadata_to_json([]) -> [];
convert_metadata_to_json([Head | Tail]) ->
    Metadata = imersia_misc:record_to_json(Head#metadata{value = imersia_misc:safely_decode_json(Head#metadata.value, return_value)}, true),
    [Metadata | convert_metadata_to_json(Tail)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, id := {_, GeneralID}, key := Key, userid := UserID, metadataid := MetadataIDFromHeader}} = Req,

    % Grab the new or updated Metadata details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(metadata, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(metadata, Req, State);
        Metadata ->
            MetadataID = get_metadata_id_from_record(Connection, GeneralID, Key, MetadataIDFromHeader, Metadata),
            NewKey = case MetadataID of
                undefined ->
                    case Key of
                        undefined -> Metadata#metadata.key;
                        Val -> Val
                    end;
                _ -> Metadata#metadata.key
            end,

            case UserID of
                % The user has to be logged in
                undefined ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(sessionid, Req, State);

                % Either create a new Metadata or update an existing one
                _ -> set_metadata(Connection, GeneralID, MetadataID, NewKey, Metadata, Req, State)
            end
    end.

% Set the metadata either given the MetadataID or Key
set_metadata(Connection, _, undefined, undefined, _, Req, State) ->
    imersia_db:close(Connection),
    imersia_misc:response_error(key, Req, State);
set_metadata(Connection, GeneralID, MetadataID, undefined, Metadata, Req, State) ->
    case imersia_db:metadata_set_by_id(Connection, GeneralID, MetadataID, Metadata) of
        {ok, _} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{metadataid, MetadataID}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;
set_metadata(Connection, GeneralID, undefined, NewKey, Metadata, Req, State) ->
    case imersia_db:metadata_set(Connection, GeneralID, NewKey, Metadata#metadata.value) of
        {ok, MetadataID} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{metadataid, MetadataID}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;
set_metadata(Connection, GeneralID, MetadataID, _NewKey, Metadata, Req, State) ->
    case imersia_db:metadata_set_by_id(Connection, GeneralID, MetadataID, Metadata) of
        {ok, _} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{metadataid, MetadataID}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a Metadata
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters
    #{parameters := #{connection := Connection, metadataid := MetadataID, id := {_, GeneralID}}} = Req,

    % Delete the Metadata
    case imersia_db:metadata_delete_by_id(Connection, GeneralID, MetadataID) of
        {ok, deleted} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{metadataid, MetadataID}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
