% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Log a event to a Geobot
% ----------------------------------------------------------------------------------------------------
-module(imersia_geobots_log_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_accepted/2,
    from_json/2,
    resource_exists/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"POST">>, <<"OPTIONS">>], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Collect common parameters
    Connection = imersia_db:connect(),
    SessionID = cowboy_req:header(<<"sessionid">>, Req),
    GeobotID = cowboy_req:header(<<"geobotid">>, Req),
    ChannelID = get_channelid(Connection, GeobotID, cowboy_req:header(<<"channelid">>, Req)),
    UserID = get_userid(Connection, SessionID),
    OwnerID = case cowboy_req:header(<<"userid">>, Req) of undefined -> UserID; Value -> Value end,
    DeveloperID = cowboy_req:header(<<"developerid">>, Req),
    Location = cowboy_req:header(<<"location">>, Req),
    % ContextIDs define extra capabilities if not the owner of the geobot(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, contextids => ContextIDs, location => Location, sessionid => SessionID, channelid => ChannelID, userid => UserID, ownerid => OwnerID, geobotid => GeobotID},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} ->
            % Check whether the user can write to this Geobot or not
            Allowance = imersia_auth:check_geobot_attribute(Connection, post, GeobotID, UserID, ContextIDs, <<"log">>),
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
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the ChannelID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, geobotid := GeobotID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    case imersia_db:geobot_exists(Connection, GeobotID) of
        {ok, exists} -> {true, Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.

% Get the UserID from the SessionID
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.

% Get the ChannelID from the GeobotID if it isn't already defined
get_channelid(_, undefined, undefined) -> undefined;
get_channelid(Connection, GeobotID, undefined) ->
    case imersia_db:geobot_getchannelid(Connection, GeobotID) of
        {ok, ChannelID} -> ChannelID;
        _ -> undefined
    end;
get_channelid(_, _, ChannelID) -> ChannelID.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, location := Location, geobotid := GeobotID}} = Req,

    % Grab the event and parameters from then body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    Event = ej:get({<<"event">>}, Body),
    case Event of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(event, Req, State);
        _ ->
            Parameters = case ej:get({<<"parameters">>}, Body) of
                undefined -> [];
                Value -> Value
            end,

            Date = calendar:universal_time(),
            imersia_db:analytics_log(Connection, GeobotID, Event, Location, Date, Parameters),
            imersia_misc:debug(debug, "Logging event ~s from ~s with parameters ~w. ~n", [Event, GeobotID, Parameters]),
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body([<<"{\"ok\":\"logged\"}">>], Req), State}
    end.
% ----------------------------------------------------------------------------------------------------
