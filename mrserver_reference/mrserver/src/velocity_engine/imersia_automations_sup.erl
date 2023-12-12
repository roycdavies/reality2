% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Automations Supervisor for the Velocity Engine
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_automations_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/1, init/1]). %, loop/2]).

-export([load_automations/2, start_automation/2, update_automations/2, start_automations/1, kill_automation/2]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link(GeobotID) ->
    GeobotAutoSupNameAtom = process_name(GeobotID),

    imersia_misc:debug(debug, "Starting a new Automations supervisor ~s~n", [GeobotAutoSupNameAtom]),

    Result = supervisor:start_link({local, GeobotAutoSupNameAtom}, imersia_automations_sup, []),

    start_automations(GeobotID),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
process_name(GeobotID) -> binary_to_atom(<< GeobotID/binary, "_auto_sup" >>, utf8).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => simple_one_for_one, intensity => 10, period => 50},
    ChildSpec = #{
        id => imersia_automations_worker,
        start => {imersia_automations_worker, start_link, []},
        type => worker,
        restart => permanent
    },
    {ok, {RestartStrategy, [ChildSpec]}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
kill_automation(GeobotID, Connection) ->
    supervisor:terminate_child(process_name(GeobotID), Connection).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_automations(GeobotID) ->
    Connection = imersia_db:connect(),
    case imersia_db:automation_list(Connection, GeobotID) of
        {ok, Automations} ->
            load_automations(GeobotID, Automations);
        _ -> ok
    end,
    imersia_db:close(Connection),
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Loads the existing automations from the database and starts workers for each of them.
% If the Automations are already started, doesn't restart them.
% ----------------------------------------------------------------------------------------------------
load_automations(_, []) -> ok;
load_automations(GeobotID, [Automation | Automations]) ->
    start_automation(GeobotID, Automation),
    load_automations(GeobotID, Automations), ok;
load_automations(_, _) -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Start the given Automation
% ----------------------------------------------------------------------------------------------------
start_automation(GeobotID, Automation) ->
    AutomationID = Automation#automation.automationid,
    % Sends a message to the loop to create a new Automation which in turn starts a new Automation
    % process if one doesn't already exist
    AutoSupervisorName = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    case whereis(AutoSupervisorName) of
        undefined -> ok;
        _ -> AutoSupervisorName ! {update, AutomationID, Automation}
    end,
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Kill all the Automations that no longer are required, update the currently existing ones, and
% create new Automations where required.
% ----------------------------------------------------------------------------------------------------
update_automations(_, undefined) -> ok;
update_automations(GeobotID, NewAutomations) ->
    % Get the IDs of the Automations that are either new or to be updated
    NewAutomationIDs = extract_ids(NewAutomations),
    % Tell the supervisor loop to kill all the others that are not in this list
    AutoSupervisorName = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    case whereis(AutoSupervisorName) of
        undefined -> ok;
        _ ->
            % Clear any old timers lying about
            imersia_actions:clear_timers(GeobotID),
            % Kill automations that are NOT being updated
            AutoSupervisorName ! {kill, NewAutomationIDs},
            % Update or Create the Automations specified
            update_or_create_automations(GeobotID, NewAutomations)
    end,
    ok.

extract_ids(<<>>) -> [];
extract_ids([]) -> [];
extract_ids([Automation | Automations]) ->
    [Automation#automation.automationid | extract_ids(Automations)].

update_or_create_automations(_, <<>>) -> ok;
update_or_create_automations(_, []) -> ok;
update_or_create_automations(GeobotID, [NewAutomation | NewAutomations]) ->
    AutoSupervisorName = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    case whereis(AutoSupervisorName) of
        undefined -> update_or_create_automations(GeobotID, NewAutomations);
        _ ->
            AutomationID = NewAutomation#automation.automationid,
            AutoSupervisorName ! {update, AutomationID, NewAutomation},
            update_or_create_automations(GeobotID, NewAutomations)
    end.
% ----------------------------------------------------------------------------------------------------
