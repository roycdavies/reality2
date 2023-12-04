% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of automatoin functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_automation).

-include("../imersia_datatypes.hrl").

-export([
    automation_init/2, automation_exists/3, automation_drop/2, automation_id/3, automation_new/3, automation_list/2, automation_getdetails/3, automation_getcommands/2, automation_setdetails/4, automation_delete/3, automation_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Set up the table for this GeobotID
% ----------------------------------------------------------------------------------------------------
automation_init(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            mc_worker_api:ensure_index(Connection, <<"automations">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"name">>}),
            mc_worker_api:disconnect(Connection),
            {ok, created};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Automation exists
% ----------------------------------------------------------------------------------------------------
automation_exists(_Connection, GeobotID, AutomationID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"automations">>, #{<<"_id">> => AutomationID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, automationid};
                _Details ->
                    mc_worker_api:disconnect(Connection),
                    {ok, exists}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the AutomationID given the Name, if it exists
% ----------------------------------------------------------------------------------------------------
automation_id(_, _, undefined) -> {error, name};
automation_id(_, undefined, _) -> {error, id};
automation_id(_Connection, GeobotID, Name) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"automations">>, #{<<"name">> => Name}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, automationid};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    {ok, maps:get(<<"_id">>, Details)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Automation
% ----------------------------------------------------------------------------------------------------
automation_new(_Connection, GeobotID, NewAutomation) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            AutomationID = imersia_db:new_id(),
            NewAutomationMap = maps:remove(<<"automationid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(NewAutomation, false))),

            mc_worker_api:insert(Connection, <<"automations">>, NewAutomationMap#{<<"_id">> => AutomationID}),
            mc_worker_api:disconnect(Connection),
            {ok, AutomationID};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Automations for this GeobotID
% ----------------------------------------------------------------------------------------------------
automation_list(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"automations">>, {}) of
                {ok, Cursor} ->
                    AutomationList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, AutomationList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {AutomationMap}) ->
    AutomationJSON = imersia_misc:safely_convert_from_map(AutomationMap#{<<"id">> => maps:get(<<"_id">>, AutomationMap)}),
    Automation = imersia_misc:json_to_record(automation, AutomationJSON),
    [Automation | get_at_cursor(Cursor, mc_cursor:next(Cursor))].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Details of the given automation by AutomationID
% ----------------------------------------------------------------------------------------------------
automation_getdetails(_Connection, GeobotID, AutomationID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"automations">>, #{<<"_id">> => AutomationID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, automationid};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    AutomationJSON = imersia_misc:safely_convert_from_map(Details#{<<"id">> => maps:get(<<"_id">>, Details)}),
                    Automation = imersia_misc:json_to_record(automation, AutomationJSON),
                    {ok, Automation}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get all the unique commands associated with a Geobot
% ----------------------------------------------------------------------------------------------------
automation_getcommands(_Connection, GeobotID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"automations">>, {}) of
                {ok, Cursor} ->
                    AutomationCommandsList = get_all_commands(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    AutomationUniqueCommands = sets:to_list(sets:from_list(lists:flatten(AutomationCommandsList))),
                    {ok, AutomationUniqueCommands};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all_commands(Cursor) -> get_commands_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_commands_at_cursor(_, error) -> [];
get_commands_at_cursor(Cursor, {AutomationMap}) ->
    AutomationJSON = imersia_misc:safely_convert_from_map(AutomationMap#{<<"id">> => maps:get(<<"_id">>, AutomationMap)}),
    Automation = imersia_misc:json_to_record(automation, AutomationJSON),
    [Automation#automation.commands | get_commands_at_cursor(Cursor, mc_cursor:next(Cursor))].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set details of an existing Automation
% ----------------------------------------------------------------------------------------------------
automation_setdetails(_Connection, GeobotID, AutomationID, NewAutomation) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection1} ->
            CheckNull = fun(_Key, Value) -> (Value /= null) and (Value /= undefined) end,

            case mc_worker_api:find_one(Connection1, <<"automations">>, #{<<"_id">> => AutomationID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection1),
                    {error, automationid};
                _ ->
                    NewAutomationMap = maps:remove(<<"automationid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(NewAutomation, false))),

                    DBEntry = maps:filter(CheckNull, NewAutomationMap#{<<"_id">> => AutomationID}),
                    Command = #{<<"$set">> => DBEntry},

                    mc_worker_api:update(Connection1, <<"automations">>, #{<<"_id">> => AutomationID}, Command),
                    mc_worker_api:disconnect(Connection1),
                    {ok, updated}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Automation
% ----------------------------------------------------------------------------------------------------
automation_delete(_Connection, GeobotID, AutomationID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", GeobotID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"automations">>, #{<<"_id">> => AutomationID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, automationid};
                _Details ->
                    mc_worker_api:delete(Connection, <<"automations">>, #{<<"_id">> => AutomationID}),
                    mc_worker_api:disconnect(Connection),
                    {ok, deleted}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Automations for this GeobotID
% ----------------------------------------------------------------------------------------------------
automation_delete_all(Connection, GeobotID) ->
    case imersia_db:automation_list(Connection, GeobotID) of
        {ok, AutomationList} -> delete_automation_list(Connection, GeobotID, AutomationList);
        {error, Reason} -> {error, Reason}
    end.

delete_automation_list(_, _, []) -> {ok, deleted};
delete_automation_list(Connection, GeobotID, [Automation | Automations]) ->
    imersia_db:automation_delete(Connection, GeobotID, Automation#automation.automationid),
    delete_automation_list(Connection, GeobotID, Automations).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Not required for MongoDB, but included for completeness
% ----------------------------------------------------------------------------------------------------
automation_drop(_Connection, _GeobotID) -> {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
