-module(imersia_entanglements_worker_fsm).
-behaviour(gen_statem).

-include("../imersia_datatypes.hrl").

-export([start_link/1, send_event/2]). %, stop/1]).
-export([handle_event/4, init/1, callback_mode/0]).


% ----------------------------------------------------------------------------------------------------
% Set up and start Automation
% ----------------------------------------------------------------------------------------------------
start_link(EntanglementDetails) ->
    #{id := EntanglementProcessAtom} = EntanglementDetails,
    imersia_misc:debug(debug, "Starting a new Entanglement worker ~p~n", [EntanglementProcessAtom]),
    EnganglementPid = spawn_link(imersia_entanglements_worker, loop, [EntanglementDetails]),
    register(EntanglementProcessAtom, EnganglementPid),

    gen_statem:start_link({local, EntanglementProcessAtom}, imersia_entanglements_worker_fsm, [], []).

send_event(Connection, {Event, Parameters}) ->
    gen_statem:call(Connection, {Event, Parameters}).

% stop(Connection) ->
%     gen_statem:stop(Connection).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Mandatory callback functions
% ----------------------------------------------------------------------------------------------------
init([]) ->
    % Default state is init
    {ok, init, {}}.

callback_mode() -> handle_event_function.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% state callback(s)
% ----------------------------------------------------------------------------------------------------
handle_event({call, _From}, init, get_sessionid, _Data) ->
    {next_state, ok}.
