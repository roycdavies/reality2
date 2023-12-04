% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Automation functions using Mnesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_automation).

-include("../imersia_datatypes.hrl").

-export([
    automation_init/2, automation_exists/3, automation_drop/2, automation_id/3, automation_list/2, automation_new/3, automation_getdetails/3, automation_getcommands/2, automation_setdetails/4, automation_delete/3, automation_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Remove the dashes from the UUID to make it a valid table name
% ----------------------------------------------------------------------------------------------------
tablename(GeobotID) ->
    list_to_atom(binary_to_list(<<"imersia_automations_">>) ++ binary_to_list(binary:replace(GeobotID, <<"-">>, <<"">>, [global]))).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the table for this object GeobotID
% ----------------------------------------------------------------------------------------------------
automation_init(_Connection, GeobotID) ->
    _ = mnesia:create_table(tablename(GeobotID), [
        {attributes, record_info(fields, automation)},
        {record_name, automation},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    _ = mnesia:add_table_index(tablename(GeobotID), name),
    {ok, created}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Automation exists
% ----------------------------------------------------------------------------------------------------
automation_exists(_Connection, GeobotID, AutomationID) ->
    Fun = fun () -> mnesia:read(tablename(GeobotID), AutomationID) end,
    AutomationList = mnesia:transaction(Fun),

    case AutomationList of
        {atomic, []} -> {error, automationid};
        {atomic, [_]} -> {ok, exists};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the AutomationID given the Name, if it exists
% ----------------------------------------------------------------------------------------------------
automation_id(_, _, undefined) -> {error, name};
automation_id(_, undefined, _) -> {error, id};
automation_id(_Connection, GeobotID, Name) ->
    Fun = fun () -> mnesia:index_read(tablename(GeobotID), Name, #automation.name) end,
    AutomationList = mnesia:transaction(Fun),

    case AutomationList of
        {atomic, []} -> {error, name};
        {atomic, [Automation | _]} -> {ok, Automation#automation.automationid};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Automations for this GeobotID
% ----------------------------------------------------------------------------------------------------
automation_list(Connection, GeobotID) ->
    Fun = fun () -> mnesia:all_keys(tablename(GeobotID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, AutomationIDList} ->
            case process_automation_list(Connection, GeobotID, AutomationIDList) of
                {error, Reason} -> {error, Reason};
                AutomationList -> {ok, AutomationList}
            end;
        _ -> {error, database}
    end.

process_automation_list(_, _, []) -> [];
process_automation_list(Connection, GeobotID, [AutomationID | AutomationIDList]) ->
    case automation_getdetails(Connection, GeobotID, AutomationID) of
        {ok, Automation} -> [Automation | process_automation_list(Connection, GeobotID, AutomationIDList)];
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Details of the given automation AutomationID
% ----------------------------------------------------------------------------------------------------
automation_getdetails(_Connection, GeobotID, AutomationID) ->
    Fun = fun () -> mnesia:read(tablename(GeobotID), AutomationID) end,
    AutomationList = mnesia:transaction(Fun),

    case AutomationList of
        {atomic, []} -> {error, automationid};
        {atomic, [Automation]} -> {ok, Automation};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get all the unique commands associated with a Geobot
% ----------------------------------------------------------------------------------------------------
automation_getcommands(Connection, GeobotID) ->
    Fun = fun () -> mnesia:all_keys(tablename(GeobotID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, AutomationIDList} ->
            case process_commands_list(Connection, GeobotID, AutomationIDList) of
                {error, Reason} -> {error, Reason};
                AutomationCommandsList ->
                    AutomationUniqueCommands = sets:to_list(sets:from_list(lists:flatten(AutomationCommandsList))),
                    {ok, AutomationUniqueCommands}
            end;
        _ -> {error, database}
    end.

process_commands_list(_, _, []) -> [];
process_commands_list(Connection, GeobotID, [AutomationID | AutomationIDList]) ->
    case automation_getdetails(Connection, GeobotID, AutomationID) of
        {ok, Automation} -> [Automation#automation.commands | process_commands_list(Connection, GeobotID, AutomationIDList)];
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Automation Details
% ----------------------------------------------------------------------------------------------------
automation_new(_Connection, GeobotID, Automation) ->
    AutomationID = imersia_db:new_id(),
    DBEntry = #automation{
        automationid = AutomationID,
        name = Automation#automation.name,
        description = Automation#automation.description,
        commands = Automation#automation.commands,
        transitions = Automation#automation.transitions
    },
    Fun = fun () -> mnesia:write(tablename(GeobotID), DBEntry, write) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, AutomationID};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Update an existing automation
% ----------------------------------------------------------------------------------------------------
automation_setdetails(Connection, GeobotID, AutomationID, NewAutomation) ->
    case  imersia_db:automation_getdetails(Connection, GeobotID, AutomationID) of
        {ok, OldAutomation} ->
            DBEntry = imersia_misc:meld_records(OldAutomation, NewAutomation),
            Fun = fun () -> mnesia:write(tablename(GeobotID), DBEntry, write) end,
            Result = mnesia:transaction(Fun),
            case Result of
                {atomic, ok} -> {ok, updated};
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Automation
% ----------------------------------------------------------------------------------------------------
automation_delete(_Connection, GeobotID, AutomationID) ->
    Fun = fun () -> mnesia:delete({tablename(GeobotID), AutomationID}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, deleted};
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
delete_automation_list(Connection, GeobotID, [Head | Tail]) ->
    _ = imersia_db:automation_delete(Connection, GeobotID, Head#automation.automationid),
    delete_automation_list(Connection, GeobotID, Tail).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Completely removes the automation database table for this GeobotID - only used when deleting the Geobot
% ----------------------------------------------------------------------------------------------------
automation_drop(_Connection, GeobotID) ->
    _ = mnesia:delete_table(tablename(GeobotID)), {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
