% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Geobot functions using MNesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_geobot).

-include("../imersia_datatypes.hrl").

-export([
    geobot_new/4, geobot_new/5, geobot_exists/2, geobot_getchannelid/2, geobot_list/4, geobot_list/1, geobot_list/6, geobot_getdetails/2, geobot_getdetails/3, geobot_setdetails/3, geobot_delete/3, geobot_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Remove the dashes from the UUID to make it a valid table name
% ----------------------------------------------------------------------------------------------------
tablename(ChannelID) ->
    list_to_atom(binary_to_list(<<"imersia_channels_">>) ++ binary_to_list(binary:replace(ChannelID, <<"-">>, <<"">>, [global]))).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new Geobot
% ----------------------------------------------------------------------------------------------------
geobot_new(Connection, ChannelID, UserID, Details) ->
    GeobotID = imersia_db:new_id(),
    geobot_new(Connection, ChannelID, UserID, GeobotID, Details).

geobot_new(Connection, ChannelID, UserID, GeobotID, Details) ->
    DBEntry = #geobot {
        geobotid = GeobotID,
        channelid = ChannelID,
        ownerid = UserID,
        name = Details#geobot.name,
        description = Details#geobot.description,
        class = imersia_misc:convert_class(Details#geobot.class),
        imageurl = Details#geobot.imageurl,
        location = imersia_misc:unify_location(Details#geobot.location),
        radius = imersia_misc:convert_number(Details#geobot.radius),
        hidden = imersia_misc:convert_bool(Details#geobot.hidden),
        created = iso8601:format(calendar:universal_time()),
        modified = iso8601:format(calendar:universal_time())
    },
    Fun2 = fun () -> mnesia:write(tablename(ChannelID), DBEntry, write) end,
    Result = mnesia:transaction(Fun2),
    case Result of
        {atomic, ok} ->
            imersia_db:metadata_init(Connection, GeobotID),
            imersia_db:context_init(Connection, GeobotID),
            imersia_db:automation_init(Connection, GeobotID),
            geobot_new_geobot_entry(Connection, GeobotID, ChannelID, UserID, DBEntry);
        % Some miscellaneous error
        _ -> {error, database}
    end.

geobot_new_geobot_entry(_Connection, GeobotID, ChannelID, UserID, Details) ->
    DBEntry = #geobotlite {
        geobotid = GeobotID,
        channelid = ChannelID,
        ownerid = UserID,
        location = Details#geobot.location,
        radius = Details#geobot.radius
    },
    Fun2 = fun () -> mnesia:write(imersia_admin_geobots, DBEntry, write) end,
    Result = mnesia:transaction(Fun2),
    case Result of
        {atomic, ok} -> {ok, GeobotID};
        % Some miscellaneous error
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Geobot exists
% ----------------------------------------------------------------------------------------------------
geobot_exists(_Connection, undefined) -> {error, geobotid};
geobot_exists(_Connection, GeobotID) ->
    Fun = fun () -> mnesia:read(imersia_admin_geobots, GeobotID) end,
    Geobot = mnesia:transaction(Fun),

    case Geobot of
        {atomic, []} -> {error, geobotid};
        {atomic, [_]} -> {ok, exists};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Geobots for this Channel
% TODO: Optimise the selection within radius code.
% ----------------------------------------------------------------------------------------------------
% Get Geobots in a channel.
geobot_list(Connection, ChannelID, VisitorID, ShowHidden) ->
    Fun = fun () -> mnesia:all_keys(tablename(ChannelID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, GeobotIDList} ->
            case process_geobot_list(Connection, GeobotIDList, VisitorID, ShowHidden) of
                {error, Reason} -> {error, Reason};
                GeobotList -> {ok, GeobotList}
            end;
        _ -> {error, database}
    end.

% Get all Geobots on this server
geobot_list(_Connection) ->
    AllRecords = fun (Record, Acc) -> [Record | Acc] end,
    Fun = fun() -> mnesia:foldl(AllRecords, [], imersia_admin_geobots) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, GeobotList} -> {ok, extract_ids(GeobotList)};
        _ -> {error, database}
    end.

% Get a list of Geobots within a given radius from the Location specified across all channels
geobot_list(Connection, undefined, VisitorID, Location, Radius, ShowHidden) ->
    Fun = fun () -> mnesia:all_keys(imersia_admin_geobots) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, GeobotIDList} ->
            case process_geobot_list(Connection, GeobotIDList, VisitorID, Location, Radius, ShowHidden) of
                {error, Reason} -> {error, Reason};
                GeobotList -> {ok, GeobotList}
            end;
        _ -> {error, database}
    end;

% Get a list of Geobots within a given radius from the Location on the given channel
geobot_list(Connection, ChannelID, VisitorID, Location, Radius, ShowHidden) ->
    Fun = fun () -> mnesia:all_keys(tablename(ChannelID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, GeobotIDList} ->
            case process_geobot_list(Connection, GeobotIDList, VisitorID, Location, Radius, ShowHidden) of
                {error, Reason} -> {error, Reason};
                GeobotList -> {ok, GeobotList}
            end;
        _ -> {error, database}
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


extract_ids([]) -> [];
extract_ids([Geobot | Geobots]) -> [Geobot#geobotlite.geobotid | extract_ids(Geobots)].

process_geobot_list(_, [], _, _) -> [];
process_geobot_list(Connection, [GeobotID | Geobots], VisitorID, ShowHidden) ->
    case imersia_db:geobot_getdetails(Connection, GeobotID) of
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
    case imersia_db:geobot_getdetails(Connection, GeobotID) of
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

% process_geobot_list(Connection, Geobots, VisitorID, Location, Radius, ShowHidden) ->
%     Test_Distance = fun (GeobotID) ->
%         case imersia_db:geobot_getdetails(Connection, GeobotID) of
%             {ok, GeobotRecord} ->
%                 if
%                     ((ShowHidden and GeobotRecord#geobot.hidden and (VisitorID == GeobotRecord#geobot.ownerid)) or (not GeobotRecord#geobot.hidden)) ->
%                         test_distance(Location, GeobotRecord#geobot.location, GeobotRecord#geobot.radius, Radius);
%                     true ->
%                         false
%                 end;
%             {error, _} -> false
%         end
%     end,
%     lists:map(Test_Distance/1, Geobots).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
geobot_getchannelid(_Connection, GeobotID) ->
    Fun = fun () -> mnesia:read(imersia_admin_geobots, GeobotID) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, []} -> {error, geobotid};
        {atomic, [Geobot]} -> {ok, Geobot#geobotlite.channelid};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Get the details of the given Geobot
% ----------------------------------------------------------------------------------------------------
geobot_getdetails(Connection, GeobotID) ->
    CHResult = geobot_getchannelid(Connection, GeobotID),
    case CHResult of
        {ok, ChannelID} ->
            Fun = fun () -> mnesia:read(tablename(ChannelID), GeobotID) end,
            Result = mnesia:transaction(Fun),

            case Result of
                {atomic, []} -> {error, geobotid};
                {atomic, [Geobot]} -> {ok, Geobot};
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.

geobot_getdetails(Connection, GeobotID, VisitorID) ->
    CHResult = geobot_getchannelid(Connection, GeobotID),
    case CHResult of
        {ok, ChannelID} ->
            Fun = fun () -> mnesia:read(tablename(ChannelID), GeobotID) end,
            Result = mnesia:transaction(Fun),

            case Result of
                {atomic, []} -> {error, geobotid};
                {atomic, [Geobot]} ->
                    if
                        ((VisitorID == Geobot#geobot.ownerid) or (not Geobot#geobot.hidden)) ->
                            {ok, Geobot};
                        true ->
                            {error, geobotid}
                    end;
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the details for the given Geobot
% ----------------------------------------------------------------------------------------------------
geobot_setdetails(Connection, GeobotID, Details) ->
    NewRecord = case imersia_misc:unify_location(Details#geobot.location) of
        null -> Details#geobot{modified=iso8601:format(calendar:universal_time())};
        Location -> Details#geobot{modified=iso8601:format(calendar:universal_time()), location=Location}
    end,

    CHResult = geobot_getchannelid(Connection, GeobotID),
    case CHResult of
        {ok, ChannelID} ->
            Fun = fun () -> mnesia:read(tablename(ChannelID), GeobotID) end,
            Result = mnesia:transaction(Fun),
            case Result of
                {atomic, []} -> {error, geobotid};
                {atomic, [OldRecord]} ->
                    DBEntry = imersia_misc:meld_records(OldRecord, NewRecord),
                    Fun2 = fun () -> mnesia:write(tablename(ChannelID), DBEntry, write) end,
                    Result2 = mnesia:transaction(Fun2),
                    case Result2 of
                        {atomic, ok} -> update_geobot_entry(Connection, GeobotID, Details);
                        _ -> {error, database}
                    end;
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.

update_geobot_entry(_Connection, GeobotID, Details) ->
    Fun = fun () -> mnesia:read(imersia_admin_geobots, GeobotID) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> {error, geobotid};
        {atomic, [OldRecord]} ->
            case imersia_misc:unify_location(Details#geobot.location) of
                null -> {ok, updated};
                Location ->
                    DBEntry = OldRecord#geobotlite{location = Location},
                    Fun2 = fun () -> mnesia:write(imersia_admin_geobots, DBEntry, write) end,
                    Result2 = mnesia:transaction(Fun2),
                    case Result2 of
                        {atomic, ok} -> {ok, updated};
                        _ -> {error, database}
                    end
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Table entry from the Geobot table
% ----------------------------------------------------------------------------------------------------
geobot_delete(Connection, ChannelID, GeobotID) ->
    case imersia_db:metadata_drop(Connection, GeobotID) of
        {ok, deleted} ->
            case imersia_db:automation_drop(Connection, GeobotID) of
                {ok, deleted} ->
                    Fun = fun () -> mnesia:delete({tablename(ChannelID), GeobotID}) end,
                    Result = mnesia:transaction(Fun),
                    case Result of
                        {atomic, ok} ->
                            Fun2 = fun() -> mnesia:delete({imersia_admin_geobots, GeobotID}) end,
                            Result2 = mnesia:transaction(Fun2),
                            case Result2 of
                                {atomic, ok} -> {ok, deleted};
                                _ -> {error, database}
                            end;
                        _ -> {error, geobotid}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Geobots in a Channel
% ----------------------------------------------------------------------------------------------------
geobot_delete_all(Connection, ChannelID) ->
    Fun = fun () -> mnesia:all_keys(tablename(ChannelID)) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, Geobots} -> geobot_delete_list(Connection, Geobots, ChannelID);
        _ -> {error, database}
    end.

geobot_delete_list(_, [], _) -> {ok, deleted};
geobot_delete_list(Connection, [GeobotID | Tail], ChannelID) ->
    Result = imersia_db:geobot_delete(Connection, ChannelID, GeobotID),
    case Result of
        {ok, deleted} -> geobot_delete_list(Connection, Tail, ChannelID);
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
