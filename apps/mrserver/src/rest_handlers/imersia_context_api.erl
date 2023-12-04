% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Context Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_context_api).

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
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> {true, Req, State};
        _ ->
            % Collect common parameters
        	Connection = imersia_db:connect(),
        	SessionID = cowboy_req:header(<<"sessionid">>, Req),
        	ID = imersia_misc:id_type(cowboy_req:header(<<"channelid">>, Req), cowboy_req:header(<<"geobotid">>, Req), undefined),
            % The ContextID to potentially operate on
            ContextID = cowboy_req:header(<<"contextid">>, Req),

            % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
            ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
            ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

            % And the User's ID from the SessionID
        	UserID = get_userid(Connection, SessionID),

            % Add them to the Request passing through
            Parameters = #{connection => Connection, sessionid => SessionID, userid => UserID, id => ID, contextid => ContextID},
            Req2 = Req#{parameters => Parameters},

            % Continue if this is a valid command for this developerID
            DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
            Location = cowboy_req:header(<<"location">>, Req2),
            case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
                {ok, logged} ->
                    Allowance = check_authorization(Connection, cowboy_req:method(Req2), ID, UserID, ContextIDs),
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
            end
    end.

% To add a new context to a Channel, you have to be able to edit the Channel
check_authorization(Connection, <<"POST">>, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel_attribute(Connection, put, ChannelID, UserID, ContextIDs, <<"edit">>);
% To delete a context, you have to be able to edit the Channel
check_authorization(Connection, <<"DELETE">>, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel_attribute(Connection, delete, ChannelID, UserID, ContextIDs, <<"edit">>);
% GET, PUT, HEAD, OPTIONS same as for Channel
check_authorization(Connection, Method, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, Method, ChannelID, UserID, ContextIDs);

% To add a new context to a Geobot, you have to be able to add a Geobot to a Channel
check_authorization(Connection, <<"POST">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    case imersia_db:geobot_getchannelid(Connection, GeobotID) of
        {ok, ChannelID} ->
            imersia_auth:check_channel_attribute(Connection, put, ChannelID, UserID, ContextIDs, <<"add">>);
        _ -> {error, geobotid}
    end;
% To edit an existing context, you have to be able to edit the Geobot
check_authorization(Connection, <<"PUT">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot_attribute(Connection, put, GeobotID, UserID, ContextIDs, <<"edit">>);
% To delete a context, you have to be able to edit the Geobot
check_authorization(Connection, <<"DELETE">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot_attribute(Connection, delete, GeobotID, UserID, ContextIDs, <<"edit">>);
% GET, HEAD, OPTIONS as for Geobot
check_authorization(Connection, Method, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, Method, GeobotID, undefined, UserID, ContextIDs);
% Otherwise an error
check_authorization(_, _, _, _, _) -> {error, id}.

% Get the UserID from the SessionID
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the item ID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    Method = cowboy_req:method(Req),
    case do_check_developerid(Req, State) of
        {true, _, _} -> do_resource_exists(Method, Req, State);
        Error -> Error
    end.

do_resource_exists(<<"HEAD">>, Req, State) ->
    #{parameters := #{connection:= Connection, contextid := ContextID, id := {_, GeneralID}}} = Req,
    case ContextID of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(contextid, Req, State);
        _ ->
            case imersia_db:context_exists(Connection, GeneralID, ContextID) of
                {ok, exists} ->
                    {true, Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end;

do_resource_exists(<<"DELETE">>, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, contextid := ContextID}} = Req,
    case ContextID of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(contextid, Req, State);
        _ -> {true, Req, State}
    end;

do_resource_exists(<<"GET">>, Req, State) ->
    #{parameters := #{connection:= Connection, contextid := ContextID, id := {_, GeneralID}}} = Req,
    case ContextID of
        undefined -> % Get list of contexts
            {true, Req, State};
        _ ->
            case imersia_db:context_exists(Connection, GeneralID, ContextID) of
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
check_existance(_, _, _) -> {error, id}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, id := {_, GeneralID}, contextid := ContextID}} = Req,
    % Get the context value
    get_context(Connection, GeneralID, ContextID, Req, State).

% No ContextID defined, so send a list of all Contexts
get_context(Connection, GeneralID, undefined, Req, State) ->
    case imersia_db:context_list(Connection, GeneralID) of
        {ok, ContextList} ->
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(convert_context_to_json(ContextList)), Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason}]), Req, State}
    end;
% Valid ContextID
get_context(Connection, GeneralID, ContextID, Req, State) ->
    case imersia_db:context_get(Connection, GeneralID, ContextID) of
        {ok, Context} ->
            ContextJSON = imersia_misc:record_to_json(Context#context{attributes = imersia_misc:safely_decode_json(Context#context.attributes, return_value)}, true),
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(ContextJSON), Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason}]), Req, State}
    end.

convert_context_to_json([]) -> [];
convert_context_to_json([Head | Tail]) ->
    Context = imersia_misc:record_to_json(Head#context{attributes = imersia_misc:safely_decode_json(Head#context.attributes, return_value)}, true),
    [Context | convert_context_to_json(Tail)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, id := {_, GeneralID}, userid := UserID, contextid := ContextIDFromHeader}} = Req,

    % Grab the new or updated Context details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(context, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(context, Req, State);
        Context ->
            case test_valid_attributes(Context#context.attributes) of
                true ->
                    ContextID = get_context_id_from_record(ContextIDFromHeader, Context),
                    case UserID of
                        % The user has to be logged in
                        undefined ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(sessionid, Req, State);

                        % Either create a new Context or update an existing one
                        _ -> set_context(Connection, GeneralID, ContextID, Context, Req, State)
                    end;
                false ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(attributes, Req, State)
            end
    end.

test_valid_attributes(Attributes) when is_list(Attributes) ->
    check_attributes_list_entities(Attributes);
test_valid_attributes(_) -> false.

check_attributes_list_entities([]) -> true;
check_attributes_list_entities([Head | Tail]) ->
    is_binary(Head) and check_attributes_list_entities(Tail).

% Get the context ID from Context Record or the Header
get_context_id_from_record(undefined, Context) ->
    case Context#context.contextid of
        undefined -> undefined;
        null -> undefined;
        <<>> -> undefined;
        MetadataID -> MetadataID
    end;
get_context_id_from_record(ContextIDFromHeader, _) ->
    ContextIDFromHeader.

% Set the context either given the ContextID
set_context(Connection, undefined, _, _, Req, State) ->
    imersia_db:close(Connection),
    imersia_misc:response_error(id, Req, State);
set_context(Connection, GeneralID, ContextID, Context, Req, State) ->
    case imersia_db:context_set(Connection, GeneralID, ContextID, Context#context.attributes) of
        {ok, ContextID2} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body([<<"{\"contextid\":\"">>, ContextID2, <<"\"}">>], Req), State};
            % {true, cowboy_req:set_resp_body(jsx:encode([{contextid, ContextID2}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a Context
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters
    #{parameters := #{connection := Connection, contextid := ContextID, id := {_, GeneralID}}} = Req,

    % Delete the Context
    case imersia_db:context_delete(Connection, GeneralID, ContextID) of
        {ok, deleted} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body(jsx:encode([{contextid, ContextID}]), Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
