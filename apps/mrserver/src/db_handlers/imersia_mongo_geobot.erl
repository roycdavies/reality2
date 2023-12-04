% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Geobot functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_geobot).

-include("../imersia_datatypes.hrl").

-export([
    geobot_new/4, geobot_new/5, geobot_exists/2, geobot_getchannelid/2, geobot_list/4, geobot_list/1, geobot_list/6, geobot_getdetails/2, geobot_getdetails/3, geobot_setdetails/3, geobot_delete/3, geobot_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Create a new Geobot
% ----------------------------------------------------------------------------------------------------
geobot_new(Connection, ChannelID, UserID, Details) ->
    GeobotID = imersia_db:new_id(),
    geobot_new(Connection, ChannelID, UserID, GeobotID, Details).

geobot_new(_Connection, ChannelID, UserID, GeobotID, Details) ->
        case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            DBEntry = #geobot {
                geobotid = GeobotID,
                channelid = ChannelID,
                ownerid = UserID,
                name = Details#geobot.name,
                description = Details#geobot.description,
                class = imersia_misc:convert_class(Details#geobot.class),
                imageurl = Details#geobot.imageurl,
                location = imersia_misc:unify_location(Details#geobot.location),
                radius = imersia_misc:convert_number(Details#geobot.radius, 0),
                hidden = imersia_misc:convert_bool(Details#geobot.hidden, false),
                created = iso8601:format(calendar:universal_time()),
                modified = iso8601:format(calendar:universal_time())
            },
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
                    imersia_db:metadata_init(Connection3, GeobotID),
                    imersia_db:context_init(Connection3, GeobotID),
                    imersia_db:automation_init(Connection3, GeobotID),
                    mc_worker_api:disconnect(Connection2),
                    {ok, GeobotID};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {error, database}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Geobot exists
% ----------------------------------------------------------------------------------------------------
geobot_exists(_Connection, undefined) -> {error, geobotid};
geobot_exists(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"geobots">>, #{<<"_id">> => GeobotID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, geobotid};
                _Details ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, exists}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Geobots for this Channel
% TODO: Optimise the selection within radius code.
% ----------------------------------------------------------------------------------------------------
% Get Geobots in a channel.
geobot_list(Connection, ChannelID, VisitorID, ShowHidden) ->
    case imersia_db:channel_exists(Connection, ChannelID) of
        {ok, exists} ->
            case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
                {ok, Connection2} ->
                    case mc_worker_api:find(Connection2, <<"geobots">>, #{<<"channelid">> => ChannelID}) of
                        {ok, Cursor} ->
                            GeobotIDList = get_all(Cursor),
                            mc_cursor:close(Cursor),
                            mc_worker_api:disconnect(Connection2),
                            case process_geobot_list(Connection, GeobotIDList, VisitorID, ShowHidden) of
                                {error, Reason} -> {error, Reason};
                                GeobotList -> {ok, GeobotList}
                            end;
                        _ ->
                            mc_worker_api:disconnect(Connection2),
                            {ok, []}
                    end;
                _ -> {error, database}
            end;
        _ -> {error, channelid}
    end.

% Get all Geobots on this server (for the Velocity Engine)
geobot_list(_Connection) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"geobots">>, {}) of
                {ok, Cursor} ->
                    GeobotIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    {ok, GeobotIDList};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {ChannelMap}) ->
    [maps:get(<<"_id">>, ChannelMap) | get_at_cursor(Cursor, mc_cursor:next(Cursor))].

% Get a list of Geobots within a given radius from the Location specified across all channels
geobot_list(Connection, undefined, VisitorID, Location, Radius, ShowHidden) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"geobots">>, {}) of
                {ok, Cursor} ->
                    GeobotIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    case process_geobot_list(Connection, GeobotIDList, VisitorID, Location, Radius, ShowHidden) of
                        {error, Reason} -> {error, Reason};
                        GeobotList -> {ok, GeobotList}
                    end;
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, []}
            end;
        _ -> {error, database}
    end;

% Get a list of Geobots within a given radius from the Location on the given channel
geobot_list(Connection, ChannelID, VisitorID, Location, Radius, ShowHidden) ->
    case imersia_db:channel_exists(Connection, ChannelID) of
        {ok, exists} ->
            case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
                {ok, Connection2} ->
                    case mc_worker_api:find(Connection2, <<"geobots">>, #{<<"channelid">> => ChannelID}) of
                        {ok, Cursor} ->
                            GeobotIDList = get_all(Cursor),
                            mc_cursor:close(Cursor),
                            mc_worker_api:disconnect(Connection2),
                            case process_geobot_list(Connection, GeobotIDList, VisitorID, Location, Radius, ShowHidden) of
                                {error, Reason} -> {error, Reason};
                                GeobotList -> {ok, GeobotList}
                            end;
                        _ ->
                            mc_worker_api:disconnect(Connection2),
                            {ok, []}
                    end;
                _ -> {error, database}
            end;
        _ -> {error, channelid}
    end.

test_distance(_, _, 0, 0) -> true;
test_distance(Location1, Location2, Radius, 0) ->
    PointDistance = imersia_misc:distance(Location1#location.latitude, Location1#location.longitude, Location2#location.latitude, Location2#location.longitude),
    (PointDistance =< Radius);
test_distance(Location1, Location2, 0, Distance) ->
    imersia_misc:distance(Location1#location.latitude, Location1#location.longitude, Location2#location.latitude, Location2#location.longitude) =< Distance;
test_distance(Location1, Location2, Radius, Distance) ->
    PointDistance = imersia_misc:distance(Location1#location.latitude, Location1#location.longitude, Location2#location.latitude, Location2#location.longitude),
    ((PointDistance =< Distance) and (PointDistance =< Radius)).

process_geobot_list(_, [], _, _) -> [];
process_geobot_list(Connection, [GeobotID | Geobots], VisitorID, ShowHidden) ->
    case geobot_getdetails(Connection, GeobotID) of
        {ok, GeobotRecord} ->
            if
                ((ShowHidden and GeobotRecord#geobot.hidden and (VisitorID == GeobotRecord#geobot.ownerid)) or (not GeobotRecord#geobot.hidden)) ->
                    [GeobotRecord | process_geobot_list(Connection, Geobots, VisitorID, ShowHidden)];
                true ->
                    process_geobot_list(Connection, Geobots, VisitorID, ShowHidden)
            end;
        {error, Reason} -> {error, Reason}
    end.

process_geobot_list(_, [], _, _, _, _) -> [];
process_geobot_list(Connection, [GeobotID | Geobots], VisitorID, Location, Radius, ShowHidden) ->
    case geobot_getdetails(Connection, GeobotID) of
        {ok, GeobotRecord} ->
            if
                ((ShowHidden and GeobotRecord#geobot.hidden and (VisitorID == GeobotRecord#geobot.ownerid)) or (not GeobotRecord#geobot.hidden)) ->
                    case test_distance(Location, GeobotRecord#geobot.location, GeobotRecord#geobot.radius, Radius) of
                        true ->
                            [GeobotRecord | process_geobot_list(Connection, Geobots, VisitorID, Location, Radius, ShowHidden)];
                        false ->
                            process_geobot_list(Connection, Geobots, VisitorID, Location, Radius, ShowHidden)
                    end;
                true ->
                    process_geobot_list(Connection, Geobots, VisitorID, Location, Radius, ShowHidden)
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the ChannelID of the Geobot
% ----------------------------------------------------------------------------------------------------
geobot_getchannelid(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"geobots">>, #{<<"_id">> => GeobotID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, geobotid};
                Details ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, maps:get(<<"channelid">>, Details)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the details of the given Geobot
% ----------------------------------------------------------------------------------------------------
geobot_getdetails(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"details">>, #{<<"_id">> => GeobotID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, geobotid};
                Details ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, geobot_convert_to_record(Details)}
            end;
        _ -> {error, database}
    end.

geobot_getdetails(_Connection, GeobotID, VisitorID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"details">>, #{<<"_id">> => GeobotID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, channelid};
                Details ->
                    GeobotRecord = geobot_convert_to_record(Details),
                    mc_worker_api:disconnect(Connection2),
                    if
                        ((VisitorID == GeobotRecord#geobot.ownerid) or (not GeobotRecord#geobot.hidden)) ->
                            {ok, GeobotRecord};
                        true ->
                            {error, geobotid}
                    end
            end;
        _ -> {error, database}
    end.

geobot_convert_to_record(DetailsMap) ->
    LocationMap = maps:get(<<"location">>, DetailsMap),
    #geobot{
        geobotid = maps:get(<<"_id">>, DetailsMap),
        channelid = maps:get(<<"channelid">>, DetailsMap),
        ownerid = maps:get(<<"ownerid">>, DetailsMap),
        class = maps:get(<<"class">>, DetailsMap),
        name = maps:get(<<"name">>, DetailsMap),
        description = maps:get(<<"description">>, DetailsMap),
        imageurl = maps:get(<<"imageurl">>, DetailsMap),
        radius = maps:get(<<"radius">>, DetailsMap),
        location = #location{
            latitude = maps:get(<<"latitude">>, LocationMap),
            longitude = maps:get(<<"longitude">>, LocationMap),
            altitude = maps:get(<<"altitude">>, LocationMap),
            geohash = maps:get(<<"geohash">>, LocationMap)
        },
        hidden = maps:get(<<"hidden">>, DetailsMap),
        created = iso8601:parse(maps:get(<<"created">>, DetailsMap)),
        modified = iso8601:parse(maps:get(<<"modified">>, DetailsMap))
    }.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the details for the given Geobot
% ----------------------------------------------------------------------------------------------------
geobot_setdetails(_Connection, GeobotID, Details) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection1} ->
            case mc_worker_api:find_one(Connection1, <<"geobots">>, #{<<"_id">> => GeobotID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection1),
                    {error, geobotid};
                _ ->
                    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
                        {ok, Connection2} ->
                            CheckNull = fun(_Key, Value) -> (Value /= null) and (Value /= undefined) end,
                            Location = imersia_misc:unify_location(Details#geobot.location),
                            GeobotDetails = maps:filter(CheckNull, #{
                                <<"modified">> => iso8601:format(calendar:universal_time()),
                                <<"class">> => Details#geobot.class,
                                <<"name">> => Details#geobot.name,
                                <<"description">> => Details#geobot.description,
                                <<"imageurl">> => Details#geobot.imageurl,
                                <<"hidden">> => imersia_misc:convert_bool(Details#geobot.hidden, null),
                                <<"radius">> => imersia_misc:convert_number(Details#geobot.radius, null),
                                <<"location.latitude">> => location_map_or_null(Location, latitude),
                                <<"location.longitude">> => location_map_or_null(Location, longitude),
                                <<"location.altitude">> => location_map_or_null(Location, altitude),
                                <<"location.geohash">> => location_map_or_null(Location, geohash)
                            }),
                            Command = #{<<"$set">> => GeobotDetails},
                            mc_worker_api:update(Connection2, <<"details">>, #{<<"_id">> => GeobotID}, Command),
                            mc_worker_api:disconnect(Connection2),

                            AdminDetails = maps:filter(CheckNull, #{
                                <<"location.coordinates.0">> => location_map_or_null(Location, longitude),
                                <<"location.coordinates.1">> => location_map_or_null(Location, latitude),
                                <<"location.coordinates.2">> => location_map_or_null(Location, altitude)
                            }),
                            Command2 = #{<<"$set">> => AdminDetails},
                            mc_worker_api:update(Connection1, <<"geobots">>, #{<<"_id">> => GeobotID}, Command2),
                            mc_worker_api:disconnect(Connection1),
                            {ok, updated};
                        _ -> {error, database}
                    end
            end;
        _ -> {error, database}
    end.

location_map_or_null(null, _) -> null;
location_map_or_null(Location, latitude) -> Location#location.latitude;
location_map_or_null(Location, longitude) -> Location#location.longitude;
location_map_or_null(Location, altitude) -> Location#location.altitude;
location_map_or_null(Location, geohash) -> Location#location.geohash.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Table entry from the Geobot table
% ----------------------------------------------------------------------------------------------------
geobot_delete(_Connection, _ChannelID, GeobotID) ->
    {ok, Connection2} = mc_worker_api:connect ([{database, <<"imersia_admin">>}]),
    mc_worker_api:delete(Connection2, <<"geobots">>, #{<<"_id">> => GeobotID}),
    mc_worker_api:disconnect(Connection2),

    % Drop the geobot's database
    {ok, Connection3} = mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]),
    mc_worker_api:command(Connection3, #{<<"dropDatabase">> => 1}),
    mc_worker_api:disconnect(Connection3),
    {ok, deleted}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Geobots in a Channel
% ----------------------------------------------------------------------------------------------------
geobot_delete_all(Connection, ChannelID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"geobots">>, #{<<"channelid">> => ChannelID}) of
                {ok, Cursor} ->
                    GeobotIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    process_geobot_delete(Connection, GeobotIDList, ChannelID);
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, deleted}
            end;
        _ -> {error, database}
    end.

process_geobot_delete(_, [], _) -> {ok, deleted};
process_geobot_delete(Connection, [GeobotID | GeobotIDs], ChannelID) ->
    case imersia_db:geobot_delete(Connection, ChannelID, GeobotID) of
        {ok, deleted} -> process_geobot_delete(Connection, GeobotIDs, ChannelID);
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
