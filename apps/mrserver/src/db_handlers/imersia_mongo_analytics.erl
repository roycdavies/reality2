% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of analytics functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_analytics).

-include("../imersia_datatypes.hrl").

-export([ analytics_log/6, analytics_query/5 ]).


% ----------------------------------------------------------------------------------------------------
% Create a new analytics table
% ----------------------------------------------------------------------------------------------------
analytics_new_table(_Connection, EntityID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", EntityID/binary>>}]) of
        {ok, Connection} ->
            mc_worker_api:ensure_index(Connection, <<"analytics">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"statid">>}),
            mc_worker_api:ensure_index(Connection, <<"analytics">>, #{<<"key">> => #{<<"index">> => 2}, <<"name">> => <<"created">>}),
            mc_worker_api:ensure_index(Connection, <<"analytics">>, #{<<"key">> => #{<<"index">> => 3}, <<"name">> => <<"geohash">>}),
            mc_worker_api:ensure_index(Connection, <<"analytics">>, #{<<"key">> => #{<<"index">> => 4}, <<"name">> => <<"event">>}),
            mc_worker_api:disconnect(Connection),
            {ok, EntityID};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Log an event with location, date and parameters, into the analytics database
% ----------------------------------------------------------------------------------------------------
analytics_log(_, EntityID, Event, Geohash, Date, Parameters) ->

    % Get the Stats ID
    {StatID, Location, ShortDate, EventLower} = imersia_db:create_statid(Event, Geohash, Date),

    % Check if an entry already exists
    case mc_worker_api:connect ([{database, <<"imersia_", EntityID/binary>>}]) of
        {ok, Connection} ->
            % Create table if it doesn't already exist
            analytics_new_table(Connection, EntityID),

            case mc_worker_api:find_one(Connection, <<"analytics">>, #{<<"statid">> => StatID}) of
                % if an entry doesn't already exist, create a new one, setting tally to 1, and adding the parameters to the list
                undefined ->
                    AnalyticID = imersia_db:new_id(),
                    DBEntry = #analytic {
                        analyticid = AnalyticID,
                        statid = StatID,
                        created = ShortDate,
                        location = Location,
                        event = EventLower,
                        tally = 1,
                        params = setstats([], Parameters),
                        indexes = setindexes([], Parameters, 0)
                    },

                    DBEntryMap = maps:remove(<<"analyticid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),
                    mc_worker_api:insert(Connection, <<"analytics">>, DBEntryMap#{<<"_id">> => AnalyticID}),
                    mc_worker_api:disconnect(Connection),
                    {ok, AnalyticID};
                Entry ->
                    % Get the details, increase the tally, and add the parameters to the list
                    AnalyticID = maps:get(<<"_id">>, Entry),
                    DBEntry2 = #{
                        <<"tally">> => imersia_misc:convert_number(maps:get(<<"tally">>, Entry)) + 1,
                        <<"params">> => setstats(maps:get(<<"params">>, Entry), Parameters),
                        <<"indexes">> => setindexes(maps:get(<<"indexes">>, Entry), Parameters, imersia_misc:convert_number(maps:get(<<"tally">>, Entry)))
                    },
                    Command = #{<<"$set">> => DBEntry2},
                    mc_worker_api:update(Connection, <<"analytics">>, #{<<"_id">> => AnalyticID}, Command),
                    mc_worker_api:disconnect(Connection),
                    {ok, AnalyticID}
            end;
        _ -> {error, database}
    end.

setstats(List, null) -> List;
setstats(List, undefined) -> List;
setstats(List, []) -> List;
setstats(List, NewEntry) -> [NewEntry | List].

setindexes(List, null, _) -> List;
setindexes(List, undefined, _) -> List;
setindexes(List, [], _) -> List;
setindexes(List, _, Index) -> [Index | List].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Query the analytics database, selecting first by the EntityID, then by Event, and then by dates
% ----------------------------------------------------------------------------------------------------
% Undefined Event - choose all events
analytics_query(_Connection, EntityID, undefined, Startdate, Enddate) ->
    case mc_worker_api:connect ([{database, <<"imersia_", EntityID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"analytics">>, {}) of
                {ok, Cursor} ->
                    AnalyticsList = get_all(Cursor, Startdate, Enddate),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, AnalyticsList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end;

% Find records of events between given dates
analytics_query(_Connection, EntityID, Event, Startdate, Enddate) ->
    case mc_worker_api:connect ([{database, <<"imersia_", EntityID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"analytics">>, #{<<"event">> => Event}) of
                {ok, Cursor} ->
                    AnalyticsList = get_all(Cursor, Startdate, Enddate),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, AnalyticsList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

% Process the records, returning only those within the given dates
get_all(Cursor, Startdate, Enddate) -> get_at_cursor(Cursor, mc_cursor:next(Cursor), Startdate, Enddate).
get_at_cursor(_, error, _, _) -> [];
get_at_cursor(Cursor, {AnalyticsMap}, Startdate, Enddate) ->
    Created = iso8601:parse(maps:get(<<"created">>, AnalyticsMap)),

    % Check if record should be included in output
    TimeDiff1 = calendar:datetime_to_gregorian_seconds(Created) - calendar:datetime_to_gregorian_seconds(Startdate),
    TimeDiff2 = calendar:datetime_to_gregorian_seconds(Enddate) - calendar:datetime_to_gregorian_seconds(Created),

    if
        ((TimeDiff1 >= 0) and (TimeDiff2 >= 0)) ->
            AnalyticJSON = imersia_misc:safely_convert_from_map(AnalyticsMap#{<<"id">> => maps:get(<<"_id">>, AnalyticsMap)}),
            AnalyticRecord = imersia_misc:json_to_record(analytic, AnalyticJSON),
            [AnalyticRecord | get_at_cursor(Cursor, mc_cursor:next(Cursor), Startdate, Enddate)];
        true -> get_at_cursor(Cursor, mc_cursor:next(Cursor), Startdate, Enddate)
    end.
% ----------------------------------------------------------------------------------------------------
