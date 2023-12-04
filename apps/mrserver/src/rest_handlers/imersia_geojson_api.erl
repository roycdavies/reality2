% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The GeoJSON Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_geojson_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_json/2,
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
allowed_methods(Req, State) -> {[<<"GET">>, <<"OPTIONS">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Preflight CORS parameters
% ----------------------------------------------------------------------------------------------------
options(Req, State) ->
    % Req2 = imersia_misc:add_cors(<<"GET, OPTIONS">>, Req),
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Content-Type, developerid, location, sessionid, radius, touch">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
    {ok, Req3, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) -> is_authorized(cowboy_req:method(Req), Req, State).
is_authorized(<<"GET">>, Req, State) ->
    % Collect common parameters
	Connection = imersia_db:connect(),
	SessionID = cowboy_req:header(<<"sessionid">>, Req),
    ChannelName = cowboy_req:binding(channelname, Req),
    LocationReq = cowboy_req:binding(location, Req),
    LocationHead = cowboy_req:header(<<"location">>, Req),
    #{radius := RadiusReq} = try cowboy_req:match_qs([radius], Req) of
        ResultR -> ResultR
    catch
        _:_ -> #{radius => undefined}
    end,
    RadiusHead = cowboy_req:header(<<"radius">>, Req),
    Location = select_location(LocationReq, LocationHead),
    RadiusBinary = select_radius(RadiusReq, RadiusHead),
    Radius = list_to_integer(binary_to_list(RadiusBinary)),
    % DeveloperID = <<"com.imersia.geojson">>,

    % Get the UsersID, if there is one
	UserID = get_userid(Connection, SessionID),

    % Get the channel_id, if there is a channel name
    ChannelID = case imersia_db:channel_id(Connection, ChannelName) of
        {ok, ResultC} -> ResultC;
        {error, _} -> undefined
    end,
    ListChannels = (ChannelID =:= undefined),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, sessionid => SessionID, userid => UserID, channelid => ChannelID, location => Location, radius => Radius, listchannels => ListChannels},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    % case imersia_developer:check_and_log(DeveloperID, Location, <<"GET">>, cowboy_req:uri(Req2), Parameters) of
    %     {ok, logged} ->
            if
                % Getting a list of channels
                (ChannelID == undefined) ->
                    imersia_db:close(Connection),
                    {true, Req2, State};
                % Getting a specific channel and its geobots
                true ->
                    Response = case imersia_auth:check_channel(Connection, <<"GET">>, ChannelID, public, []) of
                        {ok, _} -> true;
                        {error, Reason} -> imersia_db:close(Connection), {false, atom_to_list(Reason)}
                    end,
                    {Response, Req2, State}
            end;
    %     {error, Reason} ->
    %         imersia_db:close(Connection),
    %         {{false, atom_to_list(Reason)} , Req2, State}
    % end;

is_authorized(<<"OPTIONS">>, Req, State) ->
    {true, Req, State}.

% Get the UserID from the SessionID, if there is one
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.

% Select the location from the header or request
select_location(undefined, undefined) -> undefined;
select_location(undefined, Location) -> Location;
select_location(Location, _) -> Location.

% Select the radius from the header or request, or a default of 200 meters
select_radius(undefined, undefined) -> <<"0">>;
select_radius(undefined, Radius) -> Radius;
select_radius(Radius, _) -> Radius.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, channelid := ChannelID, location := Location, radius := Radius, listchannels := ListChannels}} = Req,
    get_channels(Req, State, Connection, ChannelID, Location, Radius, ListChannels).

% Get a list of channels and geobots
get_channels(Req, State, Connection, _, Location, Radius, true) ->
    Result = imersia_db:channel_list(Connection, undefined, undefined, false),
    case Result of
        {ok, ChannelList} ->
            SortFun = fun(A, B) -> A#channel.name < B#channel.name end,
            SortedChannelList = lists:sort(SortFun, ChannelList),
            ChannelsGeojson = convert_channels_to_json(SortedChannelList),

            Geobots = get_geobots(Connection, Location, undefined, Radius),
            GeobotsMetadata = get_geobot_metadata(Connection, Geobots),
            GeobotsCommands = get_geobot_commands(Connection, Geobots),
            GeobotsGeojson = convert_geobots_to_geojson(Geobots, GeobotsMetadata, GeobotsCommands),

            imersia_db:close(Connection),
            {jsx:encode(imersia_geojson:channels(ChannelsGeojson, GeobotsGeojson)), imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State};
        % Some odd error occurred
        {error, Reason2} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason2}]), imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State}
    end;

% Not a list of channels, and no channelname - so error
get_channels(Req, State, _, undefined, _, _, _) ->
    {{error, channelname}, imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State};

% Get Geobots on a Channel, and the Channel details
get_channels(Req, State, Connection, ChannelID, Location, Radius, false) ->
    % Get the Channel and Geobots and process the result to GeoJSON
    Result = imersia_db:channel_getdetails(Connection, ChannelID),
    case Result of
        {ok, Channel} ->
            Result2 = imersia_db:metadata_list(Connection, ChannelID),
            case Result2 of
                {ok, ChannelMetadata} ->
                    Geobots = get_geobots(Connection, Location, ChannelID, Radius),
                    GeobotsMetadata = get_geobot_metadata(Connection, Geobots),
                    GeobotsCommands = get_geobot_commands(Connection, Geobots),
                    GeobotsGeojson = convert_geobots_to_geojson(Geobots, GeobotsMetadata, GeobotsCommands),
                    imersia_db:close(Connection),
                    {jsx:encode(imersia_geojson:channel(Channel, ChannelMetadata, GeobotsGeojson)), imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {jsx:encode([{error, Reason}]), imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State}
            end;
        % Some odd error occurred
        {error, Reason2} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason2}]), imersia_misc:add_cors(<<"GET, OPTIONS">>, <<"developerid, location, sessionid, radius, touch">>, Req), State}
    end.

% Get the list of Geobots - all the Geobots on the given channel
% get_geobots(Connection, _, ChannelID, 0) ->
%     case imersia_db:geobot_list(Connection, ChannelID, undefined, false) of
%         % Return the list of Geobots TODO: Sort by distance
%         {ok, GeobotList} ->
%             SortFun = fun(A, B) -> A#geobot.name < B#geobot.name end,
%             lists:sort(SortFun, GeobotList);
%         % Some odd error occurred
%         {error, _} -> []
%     end;

% Get the list of Geobots - Geobots on the given channel within a certain radius of the location
get_geobots(Connection, GeoHash, ChannelID, Radius) ->
    Location = imersia_misc:unify_location(GeoHash),
    case imersia_db:geobot_list(Connection, ChannelID, undefined, Location, Radius, false) of
        % Return the list of Geobots TODO: Sort by distance
        {ok, GeobotList} ->
            SortFun = fun(A, B) -> A#geobot.name < B#geobot.name end,
            lists:sort(SortFun, GeobotList);
        % Some odd error occurred
        {error, _} -> []
    end.

get_geobot_metadata(_, []) -> [];
get_geobot_metadata(Connection, [Geobot | Geobots]) ->
    case imersia_db:metadata_list(Connection, Geobot#geobot.geobotid) of
        {ok, MetadataList} -> [MetadataList | get_geobot_metadata(Connection, Geobots)];
        {error, _} -> []
    end.

get_geobot_commands(_, []) -> [];
get_geobot_commands(Connection, [Geobot | Geobots]) ->
    case imersia_db:automation_getcommands(Connection, Geobot#geobot.geobotid) of
        {ok, Commands} -> [Commands | get_geobot_commands(Connection, Geobots)];
        {error, _} -> []
    end.

% get_channel_metadata(_, []) -> [];
% get_channel_metadata(Connection, [Channel | Channels]) ->
%     case imersia_db:metadata_list(Connection, Channel#geobot.channelid) of
%         {ok, MetadataList} -> [MetadataList | get_channel_metadata(Connection, Channels)];
%         {error, _} -> []
%     end.

convert_geobots_to_geojson([], _, _) -> [];
convert_geobots_to_geojson(_, [], _) -> [];
convert_geobots_to_geojson(_, _, []) -> [];
convert_geobots_to_geojson([Geobot | Geobots], [Metadata | Metadatas], [CommandsHead | CommandsTail]) ->
    [imersia_geojson:geobot(Geobot, Metadata, CommandsHead) | convert_geobots_to_geojson(Geobots, Metadatas, CommandsTail)].

convert_channels_to_json([]) -> [];
convert_channels_to_json([Head | Tail]) ->
    [imersia_misc:record_to_json(Head, false) | convert_channels_to_json(Tail)].
% ----------------------------------------------------------------------------------------------------
