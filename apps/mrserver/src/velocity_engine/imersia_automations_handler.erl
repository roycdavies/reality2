% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The clearing house of messages to Automations coming from the Geobot
% ----------------------------------------------------------------------------------------------------

-module(imersia_automations_handler).
-behaviour(gen_server).

-include("../imersia_datatypes.hrl").

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, start_link/1]).

% ----------------------------------------------------------------------------------------------------
% Required functions
% ----------------------------------------------------------------------------------------------------
init(GeobotID) ->
    Name = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    % Check of there were any Automations previously running
    AutomationStates = case application:get_env(mrserver, Name) of
        {ok, StoredAutomationPids} -> StoredAutomationPids;
        _ -> []
    end,
    % Restart them in case there was a crash
    case AutomationStates of
        % There were none being tracked, so either this is the first time or there are none that need restarting
        [] -> ok;
        % This is probably a restart after a crash, so wait a mo, then check the automations are started
        _ ->
            timer:sleep(100),
            imersia_automations_sup:start_automations(GeobotID), ok
    end,
    {ok, {GeobotID, AutomationStates}}.

start_link(GeobotID) ->
    Name = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    gen_server:start_link({local, Name}, imersia_automations_handler, GeobotID, []).

handle_call(Request, _From, State) -> process(Request, State).
handle_cast(Request, State) -> process(Request, State).
handle_info(Request, State) -> process(Request, State).
terminate(_Args, _State) -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Handle Calls to the Automations
% ----------------------------------------------------------------------------------------------------
process({update, AutomationID, NewAutomation}, {GeobotID, AutomationStates}) ->
    UpdatedAutomationStates = update_automation(GeobotID, AutomationStates, AutomationID, NewAutomation),
    Name = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    application:set_env(mrserver, Name, UpdatedAutomationStates, [{persistent, true}]),
    {noreply, {GeobotID, UpdatedAutomationStates}};

process({kill, NewAutomationIDs}, {GeobotID, AutomationStates}) ->
    NewAutomationIDsSet = sets:from_list(NewAutomationIDs),
    UpdatedAutomationStates = kill_non_existing_automations(GeobotID, AutomationStates, NewAutomationIDsSet),
    Name = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    application:set_env(mrserver, Name, UpdatedAutomationStates, [{persistent, true}]),
    {noreply, {GeobotID, UpdatedAutomationStates}};

process({storestate, AutomationID, NewState}, {GeobotID, AutomationStates}) ->
    UpdatedAutomationStates = update_state(AutomationStates, AutomationID, NewState),
    Name = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    application:set_env(mrserver, Name, UpdatedAutomationStates, [{persistent, true}]),
    {noreply, {GeobotID, UpdatedAutomationStates}};

process({status}, {GeobotID, AutomationStates}) ->
    AutomationStates = gather_states(AutomationStates),
    send_payload_to_geobot(GeobotID, status_return, AutomationStates),
    {noreply, {GeobotID, AutomationStates}};

process(Message, {GeobotID, AutomationStates}) ->
    send_event_to_automations(GeobotID, Message, AutomationStates),
    {noreply, {GeobotID, AutomationStates}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send an event to all the Automations
% ----------------------------------------------------------------------------------------------------
send_event_to_automations(_, _, []) -> [];
send_event_to_automations(GeobotID, {Event, Parameters}, [{AutomationID, State} | Automations]) ->
    imersia_misc:debug_to_logfile(GeobotID, debug, "Sending ~s~n", [Event]),
    AutomationIDAtom = binary_to_atom(AutomationID, utf8),
    case whereis(AutomationIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Automation Worker seems to have died~n", []),
            Automations;
        Pid ->
            imersia_automations_worker:send_event(Pid, {Event, Parameters}),
            send_event_to_automations(GeobotID, {Event, Parameters}, Automations),
            [{AutomationID, State} | Automations]
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send an arbitrary data payload to the Geobot associated with these Automations
% ----------------------------------------------------------------------------------------------------
send_payload_to_geobot(GeobotID, Type, Payload) ->
    GeobotIDAtom = binary_to_atom(GeobotID, utf8),
    case whereis(GeobotIDAtom) of
        undefined -> ok;
        _ -> GeobotIDAtom ! {Type, Payload}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Change the internally held state
% ----------------------------------------------------------------------------------------------------
update_state([], _, _) -> [];
update_state([{AutomationID, _State} | AutomationStates], AutomationID, NewState) ->
    [{AutomationID, NewState} | update_state(AutomationStates, AutomationID, NewState)];
update_state([AutomationState | AutomationStates], AutomationID, NewState) ->
    [AutomationState | update_state(AutomationStates, AutomationID, NewState)].
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Return just the Automation Names and States (used by the API)
% ----------------------------------------------------------------------------------------------------
gather_states([]) -> [];
gather_states([{AutomationID, State} | AutomationStates]) ->
    [{AutomationID, State} | gather_states(AutomationStates)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Goes through the automations in the list and sends an update event to the correct one,
% if it exists, otherwise, creates a new one and adds it to the list
% Returns an updated list of Automation PIDs
% ----------------------------------------------------------------------------------------------------
% We have got through all the Automations without finding the right one, so start a new one.
update_automation(GeobotID, [], _, NewAutomation) ->
    AutomationID = NewAutomation#automation.automationid,
    AutomationSupervisor = binary_to_atom(<< GeobotID/binary, "_auto_sup" >>, utf8),
    case supervisor:start_child(AutomationSupervisor, [GeobotID, NewAutomation]) of
        {ok, Pid} ->
            % Queue up the start event
            imersia_automations_worker:send_event(Pid, {<<"start">>, []}),
            % Return the started Automation process ID in the 'init' state
            [{AutomationID, <<"init">>}];
        _ -> [] % For some reason the automation didn't start
    end;
% An automation with a matching ID, so send an update event
update_automation(GeobotID, [{AutomationID, _State} | AutomationStates], AutomationID, NewAutomation) ->
    AutomationIDAtom = binary_to_atom(AutomationID, utf8),
    case whereis(AutomationIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Automation Worker seems to have died~n", []),
            update_automation(GeobotID, AutomationStates, AutomationID, NewAutomation);
        Pid ->
            % Tell the Automation process to change its internal data
            imersia_automations_worker:send_event(Pid, {'__update', NewAutomation}),
            % Queue up the Start event
            imersia_automations_worker:send_event(Pid, {<<"start">>, []}),
            % Return the updated state
            [{AutomationID, <<"init">>} | update_automation(GeobotID, AutomationStates, AutomationID, NewAutomation)]
    end;
% An Automation that doesn't have a matching name, so skip it and try the next one
update_automation(GeobotID, [AutomationState | AutomationStates], AutomationID, NewAutomation) ->
    [AutomationState | update_automation(GeobotID, AutomationStates, AutomationID, NewAutomation)].

% Goes through the automations and both kills the one named and removes it from the list
% Returns an updated list of Automation PIDs
kill_non_existing_automations(_, [], _) -> [];
kill_non_existing_automations(GeobotID, [{AutomationID, State} | Automations], AutoIDsSet) ->
    % If the current Automation is not in the set of Automations to keep alive, then kill it
    IsNotInSet = not sets:is_element(AutomationID, AutoIDsSet),
    if
        IsNotInSet ->
            AutomationIDAtom = binary_to_atom(AutomationID, utf8),
            case whereis(AutomationIDAtom) of
                undefined ->
                    kill_non_existing_automations(GeobotID, Automations, AutoIDsSet);
                Pid ->
                    imersia_automations_sup:kill_automation(GeobotID, Pid),
                    % imersia_automations_worker:stop(Pid),
                    kill_non_existing_automations(GeobotID, Automations, AutoIDsSet)
            end;
        true ->
            [{AutomationID, State} | kill_non_existing_automations(GeobotID, Automations, AutoIDsSet)]
    end.
% ----------------------------------------------------------------------------------------------------
