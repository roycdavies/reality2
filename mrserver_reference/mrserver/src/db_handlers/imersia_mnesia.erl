% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation using mnesia
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_mnesia).

-include("../imersia_datatypes.hrl").

-export([
    init/0, drop/0, connect/0, close/1
]).



% ----------------------------------------------------------------------------------------------------
% MNesia is automatically connected as it is built in, but still need to return something here for consistancy
% ----------------------------------------------------------------------------------------------------
connect() -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% 'Close' the non-existant connection, for consistency.
% ----------------------------------------------------------------------------------------------------
close(_) -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% At the start, check that the tables exist and have the right indices
% ----------------------------------------------------------------------------------------------------
init() ->
    Tables = mnesia:system_info(local_tables),
    init_tables(
        lists:member(imersia_users_users, Tables),
        lists:member(imersia_users_sessions, Tables),
        lists:member(imersia_admin_channels, Tables),
        lists:member(imersia_admin_geobots, Tables),
        lists:member(imersia_users_passcodes, Tables)
    ).

init_tables(false, B, C, D, E) ->
    Result = mnesia:create_table(imersia_users_users, [
        {attributes, record_info(fields, user)},
        {record_name, user},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    imersia_misc:debug(debug, "~0p~n", [Result]),

    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_users_users, useremail)]),
    % imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_users_users, userid)]),

    init_tables(true, B, C, D, E);

init_tables(A, false, C, D, E) ->
    Result = mnesia:create_table(imersia_users_sessions, [
        {attributes, record_info(fields, session)},
        {record_name, session},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    imersia_misc:debug(debug, "~0p~n", [Result]),

    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_users_sessions, token)]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_users_sessions, userid)]),
    init_tables(A, true, C, D, E);

init_tables(A, B, false, D, E) ->
    imersia_misc:debug(debug, "~0p~n", [mnesia:create_table(imersia_admin_channels, [
        {attributes, record_info(fields, channel)},
        {record_name, channel},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ])]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_admin_channels, name)]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_admin_channels, ownerid)]),
    init_tables(A, B, true, D, E);

init_tables(A, B, C, false, E) ->
    imersia_misc:debug(debug, "~0p~n", [mnesia:create_table(imersia_admin_geobots, [
        {attributes, record_info(fields, geobotlite)},
        {record_name, geobotlite},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ])]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_admin_geobots, channelid)]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_admin_geobots, ownerid)]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_admin_geobots, location)]),
    init_tables(A, B, C, true, E);

init_tables(A, B, C, D, false) ->
    imersia_misc:debug(debug, "~0p~n", [mnesia:create_table(imersia_users_passcodes, [
        {attributes, record_info(fields, passcode)},
        {record_name, passcode},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ])]),
    imersia_misc:debug(debug, "~0p~n", [mnesia:add_table_index(imersia_users_passcodes, useremail)]),
    init_tables(A, B, C, D, true);

init_tables(true, true, true, true, true) -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Admin tool to drop all the current tables
% ----------------------------------------------------------------------------------------------------
drop() ->
    drop_tables(mnesia:system_info(local_tables)).

drop_tables([]) -> ok;
drop_tables([schema | Tail]) ->
    drop_tables(Tail);
drop_tables([Head | Tail]) ->
    _ = mnesia:delete_table(Head),
    drop_tables(Tail).
% ----------------------------------------------------------------------------------------------------
