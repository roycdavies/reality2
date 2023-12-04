
% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Analytics Tests
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(test_analytics).

-include("../imersia_datatypes.hrl").

-export([ test/0 ]).


% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
test() ->
    Connection = imersia_db:connect(),

    Location = imersia_misc:unify_location(-35.2867407, 174.0600984, 0, null),
    {Datepart, Timepart} = calendar:universal_time(),
    DateShifted = edate:shift(Datepart, -1, months),

    Today = {Datepart, Timepart},
    AMonthAgo = {DateShifted, Timepart},

    Event1 = <<"test">>,
    Event2 = <<"hippo">>,
    EntityID = <<"0f0752d8-abe4-445b-a9ad-40e9c98b788b">>,

    erlang:display("Testing Analytics"),

    erlang:display(imersia_db:analytics_log(Connection, EntityID, Event1, Location#location.geohash, Today, <<"hello">>)),
    erlang:display(imersia_db:analytics_log(Connection, EntityID, Event1, Location#location.geohash, Today, <<"{\"test\":23}">>)),
    erlang:display(imersia_db:analytics_log(Connection, EntityID, Event1, Location#location.geohash, Today, null)),
    erlang:display(imersia_db:analytics_log(Connection, EntityID, Event2, Location#location.geohash, Today, null)),
    erlang:display(imersia_db:analytics_log(Connection, EntityID, Event2, Location#location.geohash, Today, <<"{\"test\":23}">>)),

    erlang:display(imersia_db:analytics_query(Connection, EntityID, Event1, AMonthAgo, Today)),
    erlang:display(imersia_db:analytics_query(Connection, EntityID, undefined, AMonthAgo, Today)),

    "passed".
% ----------------------------------------------------------------------------------------------------
