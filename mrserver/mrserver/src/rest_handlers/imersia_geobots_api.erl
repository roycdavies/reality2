% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Geobot Handler for the API
% TODO: Build WebApp for Channels (and Geobots)
% ----------------------------------------------------------------------------------------------------
-module(imersia_geobots_api).

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
    Radius = case cowboy_req:header(<<"radius">>, Req) of undefined -> undefined; RadiusBinary -> list_to_integer(binary_to_list(RadiusBinary)) end,
    ShowHidden = case cowboy_req:header(<<"showhidden">>, Req) of undefined -> true; Value2 -> binary_to_boolean(Value2) end,
    % ContextIDs define extra capabilities if not the owner of the geobot(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, contextids => ContextIDs, location => Location, radius => Radius, showhidden => ShowHidden, sessionid => SessionID, channelid => ChannelID, userid => UserID, ownerid => OwnerID, geobotid => GeobotID},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} ->
            % Check whether the user can access this Geobot or not
            Allowance = imersia_auth:check_geobot(Connection, cowboy_req:method(Req), GeobotID, ChannelID, UserID, ContextIDs),
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

binary_to_boolean(<<"true">>) -> true;
binary_to_boolean(<<"false">>) -> false;
binary_to_boolean(true) -> true;
binary_to_boolean(false) -> false.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the ChannelID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, action := Action, channelid := ChannelID, geobotid := GeobotID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    case Action of
        head ->
            case imersia_db:geobot_exists(Connection, GeobotID) of
                {ok, exists} ->
                    imersia_db:close(Connection),
                    {true, Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end;
        geobotlist ->
            case imersia_db:channel_exists(Connection, ChannelID) of
                {ok, exists} ->
                    {true, Req, State}; % Asking for a list of Geobots
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end;
        _ ->
            case imersia_db:channel_exists(Connection, ChannelID) of
                {ok, exists} ->
                    case imersia_db:geobot_exists(Connection, GeobotID) of
                        {ok, exists} -> {true, Req, State};
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(Reason, Req, State)
                    end;
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
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
% The response to a GET or HEAD
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    #{parameters := #{action := Action}} = Req,
    to_json_by_method(Action, Req, State).

% The HEAD request
to_json_by_method(head, Req, State) -> {"", Req, State};

% The GET request
to_json_by_method(get, Req, State) ->
    #{parameters := #{geobotid := GeobotID}} = Req,
    to_json_by_geobot(GeobotID, Req, State);

% Looking for a list of Geobots
to_json_by_method(geobotlist, Req, State) ->
    #{parameters := #{radius := Radius}} = Req,
    to_json_by_radius(Radius, Req, State).

% A specific Geobot ID is given, therefore get its details
to_json_by_geobot(GeobotID, Req, State) ->
    #{parameters := #{connection := Connection}} = Req,
    Result = imersia_db:geobot_getdetails(Connection, GeobotID),
    case Result of
        {ok, Geobot} ->
            GeobotJSON = imersia_misc:record_to_json(Geobot, false),
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(GeobotJSON), Req, State};
        % Some odd error occurred
        {error, Reason2} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason2}]), Req, State}
    end.

% Get a list of Geobots on the given Channel
to_json_by_radius(undefined, Req, State) ->
    #{parameters := #{connection := Connection, channelid := ChannelID, ownerid := OwnerID, showhidden := ShowHidden}} = Req,
    case imersia_db:geobot_list(Connection, ChannelID, OwnerID, ShowHidden) of
        % Return the list of Geobots
        {ok, GeobotList} ->
            SortFun = fun(A, B) -> A#geobot.name < B#geobot.name end,
            SortedGeobotList = lists:sort(SortFun, GeobotList),
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(convert_geobots_to_json(SortedGeobotList)), Req, State};
        % Some odd error occurred
        {error, Reason1} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason1}]), Req, State}
    end;

% Get a list of Geobots on the given Channel within a certain radius
to_json_by_radius(Radius, Req, State) ->
    #{parameters := #{connection := Connection, location := GeoHash, channelid := ChannelID, ownerid := OwnerID, showhidden := ShowHidden}} = Req,
    Location = imersia_misc:unify_location(GeoHash),
    case imersia_db:geobot_list(Connection, ChannelID, OwnerID, Location, Radius, ShowHidden) of
        % Return the list of Geobots TODO: Sort by distance
        {ok, GeobotList} ->
            SortFun = fun(A, B) -> A#geobot.name < B#geobot.name end,
            SortedGeobotList = lists:sort(SortFun, GeobotList),
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(convert_geobots_to_json(SortedGeobotList)), Req, State};
        % Some odd error occurred
        {error, Reason1} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason1}]), Req, State}
    end.

convert_geobots_to_json([]) -> [];
convert_geobots_to_json([Head | Tail]) ->
    [imersia_misc:record_to_json(Head, false) | convert_geobots_to_json(Tail)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, action := Action, location := Location, channelid := ChannelID, userid := UserID, geobotid := GeobotID}} = Req,

    % Grab the new or updated Geobot details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),

    case imersia_misc:json_to_record(geobot, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(geobot, Req, State);
        GeobotRaw ->
            Geobot = check_location(GeobotID, GeobotRaw, Location),

            case UserID of
                % The user has to be logged in
                undefined ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(sessionid, Req, State);

                % Either create a new Geobot or update an existing one
                _ ->
                    case Action of
                        post ->
                            case imersia_db:channel_getdetails(Connection, ChannelID) of
                                {ok, Channel} ->
                                    Result = imersia_db:geobot_new(Connection, ChannelID, Channel#channel.ownerid, Geobot),
                                    case Result of
                                        {ok, NewGeobotID} ->
                                            imersia_db:close(Connection),
                                            {true, cowboy_req:set_resp_body([<<"{\"geobotid\":\"">>, NewGeobotID, <<"\"}">>], Req), State};
                                        {error, Reason} ->
                                            imersia_db:close(Connection),
                                            imersia_misc:response_error(Reason, Req, State)
                                    end;
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason, Req, State)
                            end;
                        put ->
                            % This must be a PUT update to an existing Geobot
                            Result2 = imersia_db:geobot_setdetails(Connection, GeobotID, Geobot),
                            case Result2 of
                                {ok, updated} ->
                                    imersia_db:close(Connection),
                                    {true, cowboy_req:set_resp_body([<<"{\"geobotid\":\"">>, GeobotID, <<"\"}">>], Req), State};
                                {error, Reason3} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason3, Req, State)
                            end
                    end
            end
    end.

% Sets location from either the body or the location header
check_location(undefined, Geobot=#geobot{location=null}, Location) ->
    [{lat, Latitude}, {lon, Longitude}] = geohash:decode(erlang:binary_to_list(Location)),
    Geobot#geobot{location=#location{latitude=Latitude, longitude = Longitude, geohash=Location}};
check_location(_, Geobot, _) -> Geobot.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a Geobot
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters
    #{parameters := #{connection := Connection, channelid := ChannelID, geobotid := GeobotID}} = Req,

    % Delete the Geobot
    case imersia_db:geobot_delete(Connection, ChannelID, GeobotID) of
        {ok, deleted} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body([<<"{\"geobotid\":\"">>, GeobotID, <<"\"}">>], Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
