% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation using MongoDB
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_mongo).

-include("../imersia_datatypes.hrl").

-export([
    init/0, drop/0, connect/0, close/1
]).



% ----------------------------------------------------------------------------------------------------
% MongoDB connection is per database, so will be done on the fly instead.
% ----------------------------------------------------------------------------------------------------
connect() -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% 'Close' the non-existant connection, for consistency.
% ----------------------------------------------------------------------------------------------------
close(Connection) -> mc_worker_api:disconnect(Connection).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% MongoDB builds the databases and collections on the fly, so nothing really to do here except define
% indices
% ----------------------------------------------------------------------------------------------------
init() ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            mc_worker_api:ensure_index(Connection, <<"companions">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"useremail">>}),
            mc_worker_api:ensure_index(Connection, <<"channels">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"ownerid">>}),
            mc_worker_api:ensure_index(Connection, <<"channels">>, #{<<"key">> => #{<<"index">> => 2}, <<"name">> => <<"name">>}),
            mc_worker_api:ensure_index(Connection, <<"geobots">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"ownerid">>}),
            mc_worker_api:ensure_index(Connection, <<"geobots">>, #{<<"key">> => #{<<"index">> => 2}, <<"name">> => <<"channelid">>}),
            mc_worker_api:ensure_index(Connection, <<"passcodes">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"useremail">>}),
            % mc_worker_api:ensure_index(Connection, <<"geobots">>, #{<<"key">> => #{<<"index">> => 3}, <<"name">> => <<"location">>}),
            mc_worker_api:disconnect(Connection);
        _ ->
            ok
    end,
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Admin tool to drop all the current Imersia tables
% ----------------------------------------------------------------------------------------------------
drop() ->
    {ok, Connection} = mc_worker_api:connect ([{database, admin}]),
    case mc_worker_api:command(Connection, #{<<"listDatabases">> => 1, <<"nameOnly">> => true}) of
        {true, #{<<"databases">> := Databases}} ->
            drop_databases(Databases);
        {false, _} -> error
    end,
    mc_worker_api:disconnect(Connection).

drop_databases([]) -> ok;
drop_databases([#{<<"name">> := <<"imersia_", Name/binary>>} | Databases]) ->
    drop_this_database(<<"imersia_", Name/binary>>),
    drop_databases(Databases);
drop_databases([_ | Databases]) ->
    drop_databases(Databases).

drop_this_database(Name) ->
    {ok, Connection} = mc_worker_api:connect ([{database, Name}]),
    mc_worker_api:command(Connection, #{<<"dropDatabase">> => 1}),
    mc_worker_api:disconnect(Connection).
% ----------------------------------------------------------------------------------------------------
