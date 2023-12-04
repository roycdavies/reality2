% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of analytics functions using MNesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_analytics).

-include("../imersia_datatypes.hrl").

-export([ analytics_log/6, analytics_query/5 ]).



% ----------------------------------------------------------------------------------------------------
% Remove the dashes from the UUID to make it a valid table name
% ----------------------------------------------------------------------------------------------------
tablename(EntityID) ->
    list_to_atom(binary_to_list(<<"imersia_analytics_">>) ++ binary_to_list(binary:replace(EntityID, <<"-">>, <<"">>, [global]))).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new analytics table
% ----------------------------------------------------------------------------------------------------
analytics_new_table(_Connection, EntityID) ->
    TableName = tablename(EntityID),
    _ = mnesia:create_table(TableName, [
        {attributes, record_info(fields, analytic)},
        {record_name, analytic},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    _ = mnesia:add_table_index(TableName, statid),
    _ = mnesia:add_table_index(TableName, created),
    _ = mnesia:add_table_index(TableName, geohash),
    _ = mnesia:add_table_index(TableName, event),
    {ok, EntityID}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Log an event with location, date and parameters, into the analytics database
% ----------------------------------------------------------------------------------------------------
analytics_log(Connection, EntityID, Event, Geohash, Date, Parameters) ->
    % Create table if it doesn't already exist
    _ = analytics_new_table(Connection, EntityID),

    % Get the Stats ID
    {StatID, Location, ShortDate, EventLower} = imersia_db:create_statid(Event, Geohash, Date),

    % Check if an entry already exists
    Fun = fun () -> mnesia:index_read(tablename(EntityID), StatID, #analytic.statid) end,
    StatsList = mnesia:transaction(Fun),

    case StatsList of
        % if an entry doesn't already exist, create a new one, setting tally to 1, and adding the parameters to the list
        {atomic, []} ->
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
            Fun2 = fun () -> mnesia:write(tablename(EntityID), DBEntry, write) end,
            Result = mnesia:transaction(Fun2),
            case Result of
                {atomic, ok} -> {ok, AnalyticID};
                % Some miscellaneous error
                _ -> {error, database}
            end;
        {atomic, [Entry | _]} ->
            % Get the details, increase the tally, and add the parameters to the list
            DBEntry2 = #analytic {
                analyticid = Entry#analytic.analyticid,
                statid = Entry#analytic.statid,
                created = Entry#analytic.created,
                location = Entry#analytic.location,
                event = Entry#analytic.event,
                tally = Entry#analytic.tally + 1,
                params = setstats(Entry#analytic.params, Parameters),
                indexes = setindexes(Entry#analytic.indexes, Parameters, Entry#analytic.tally)
            },
            Fun2 = fun () -> mnesia:write(tablename(EntityID), DBEntry2, write) end,
            Result = mnesia:transaction(Fun2),
            case Result of
                {atomic, ok} -> {ok, Entry#analytic.analyticid};
                % Some miscellaneous error
                _ -> {error, database}
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
    Fun = fun () -> mnesia:all_keys(tablename(EntityID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, AnalyticIDList} -> {ok, process_analytic_list(read_from_keys(EntityID, AnalyticIDList), Startdate, Enddate)};
        _ -> {error, database}
    end;

% Find records of events between given dates
analytics_query(_Connection, EntityID, Event, Startdate, Enddate) ->
    Fun = fun () -> mnesia:index_read(tablename(EntityID), Event, #analytic.event) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, AnalyticList} -> {ok, process_analytic_list(AnalyticList, Startdate, Enddate)};
        _ -> {error, database}
    end.

% Get all the records given the keys
read_from_keys(_, []) -> [];
read_from_keys(EntityID, [Head | Tail]) ->
    Fun = fun () -> mnesia:read(tablename(EntityID), Head) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, [Record]} -> [Record | read_from_keys(EntityID, Tail)];
        _ -> read_from_keys(EntityID, Tail)
    end.

% Process the records, returning only those within the given dates
process_analytic_list([], _, _) -> [];
process_analytic_list([AnalyticRecord | AnalyticListRest], Startdate, Enddate) ->
    Created = iso8601:parse(AnalyticRecord#analytic.created),

    % Check if record should be included in output
    TimeDiff1 = calendar:datetime_to_gregorian_seconds(Created) - calendar:datetime_to_gregorian_seconds(Startdate),
    TimeDiff2 = calendar:datetime_to_gregorian_seconds(Enddate) - calendar:datetime_to_gregorian_seconds(Created),

    if
        ((TimeDiff1 >= 0) and (TimeDiff2 >= 0)) ->
            [AnalyticRecord | process_analytic_list(AnalyticListRest, Startdate, Enddate)];
        true -> process_analytic_list(AnalyticListRest, Startdate, Enddate)
    end.
% ----------------------------------------------------------------------------------------------------
