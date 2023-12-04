% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The core of the Velocity Engine, managing Automations, Transitions and Actions
% ----------------------------------------------------------------------------------------------------

-module(imersia_automations_worker).
-behaviour(gen_statem).

-include("../imersia_datatypes.hrl").

-export([start_link/2, send_event/2]). %, stop/1]).
-export([handle_event/4, init/1, callback_mode/0]).


% ----------------------------------------------------------------------------------------------------
% Set up and start Automation
% ----------------------------------------------------------------------------------------------------
start_link(GeobotID, Automation) ->
    AutomationID = Automation#automation.automationid,
    AutomationIDAtom = binary_to_atom(AutomationID, utf8),
    imersia_misc:debug(debug, "Starting Automation worker ~s~n", [AutomationID]),
    gen_statem:start_link({local, AutomationIDAtom}, imersia_automations_worker, [{GeobotID, Automation}], []).

send_event(Connection, {Event, Parameters}) ->
    gen_statem:call(Connection, {Event, Parameters}).

% stop(Connection) ->
%     gen_statem:stop(Connection).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Mandatory callback functions
% ----------------------------------------------------------------------------------------------------
init([{GeobotID, Automation}]) ->
    % Default state is <<"init">>
    AutoSupervisorName = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    AutomationID = Automation#automation.automationid,
    case whereis(AutoSupervisorName) of
        undefined -> imersia_misc:debug_to_logfile(GeobotID, error, "Automation Supervisor seems to have died~n", []);
        _ -> AutoSupervisorName ! {storestate, AutomationID, <<"init">>}
    end,
    {ok, <<"init">>, {GeobotID, Automation}}.

callback_mode() -> handle_event_function.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% state callback(s)
% ----------------------------------------------------------------------------------------------------
handle_event({call, From}, {'__update', NewAutomation}, _, {GeobotID, _}) ->
    % Automation details have changed, so reset the Automation, and set the new details.
    set_next_state(From, GeobotID, NewAutomation, <<"init">>);

handle_event({call, From}, {'__state'}, CurrentState, {GeobotID, Automation}) ->
    % Request information about the current states of the automations
    AutomationID = Automation#automation.automationid,
    imersia_geobot_sup_sup:notify_geobot(GeobotID, {currentstate, AutomationID, CurrentState}),
    set_next_state(From, GeobotID, Automation, CurrentState);

handle_event({call, From}, EventAndParameters, CurrentState, {GeobotID, Automation}) ->
    % Grab the transitions from the Automations structure
    Transitions = Automation#automation.transitions,
    % Go through the Transitions to find one that matches the event
    interpret_transitions(From, GeobotID, Automation, CurrentState, EventAndParameters, Transitions);

handle_event(_, _, CurrentState, {GeobotID, Automation}) ->
    % Catchall for anything else coming through
    {next_state, CurrentState, {GeobotID, Automation}, []}.

% An error in the transitions structure
interpret_transitions(From, GeobotID, Automation, CurrentState, _, undefined) ->
    set_next_state(From, GeobotID, Automation, CurrentState);

% No (more) transitions and hence no state change
interpret_transitions(From, GeobotID, Automation, CurrentState, _, []) ->
    set_next_state(From, GeobotID, Automation, CurrentState);

% Find a transition that matches
interpret_transitions(From, GeobotID, Automation, CurrentState, {EventIn, Parameters}, [Transition | Transitions]) ->
    AutomationID = Automation#automation.automationid,
    State = Transition#transition.state,
    Event = Transition#transition.event,
    NewState = Transition#transition.newstate,
    Actions = Transition#transition.actions,
    if
        ((State == CurrentState) or (State == <<"*">>)) and (EventIn == Event) ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Automation ~s is transitioning from ~s to ~s with event ~s ~n", [Automation#automation.name, CurrentState, NewState, EventIn]),
            % Do the actions
            Result = perform_actions(GeobotID, Actions, Parameters),
            % Notify the Geobot in case anybody needs to know of the change
            imersia_geobot_sup_sup:notify_geobot(GeobotID, {statechange, AutomationID, NewState, Result}),
            % Update the supervisor with this change
            set_next_state(From, GeobotID, Automation, NewState);
        true ->
            interpret_transitions(From, GeobotID, Automation, CurrentState, {EventIn, Parameters}, Transitions)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the next state whilst keeping the automation message handler informed of the state change.
% ----------------------------------------------------------------------------------------------------
set_next_state(From, GeobotID, Automation, NewState) ->
    AutomationID = Automation#automation.automationid,
    AutoSupervisorName = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
    case whereis(AutoSupervisorName) of
        undefined -> imersia_misc:debug_to_logfile(GeobotID, error, "Automation supervisor seems to be dead.~n", []);
        _ -> AutoSupervisorName ! {storestate, AutomationID, NewState}
    end,
    {next_state, NewState, {GeobotID, Automation}, [{reply, From, NewState}]}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Perform actions based on the commands held at the transition.
% ----------------------------------------------------------------------------------------------------
perform_actions(_, [], _) -> [];
perform_actions(_, undefined, _) -> [];
perform_actions(GeobotID, [Action | Actions], EventParameters) ->
    Command = Action#action.command,
    Parameters = Action#action.parameters,
    imersia_misc:debug_to_logfile(GeobotID, debug, "Command ~s received~n", [Command]),
    [imersia_actions:interpret(GeobotID, Command, Parameters, EventParameters) | perform_actions(GeobotID, Actions, EventParameters)].
% ----------------------------------------------------------------------------------------------------
