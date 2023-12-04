% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Velocity Engine Supervisor
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_geobot_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/1]).
-export([init/1]).

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_link(GeobotID) ->
    GeobotWorkerName = << GeobotID/binary, "_sup"  >>,
    GeobotWorkerNameAtom = binary_to_atom(GeobotWorkerName, utf8),

    Result = supervisor:start_link({local, GeobotWorkerNameAtom}, imersia_geobot_sup, [GeobotID]),
    imersia_misc:debug(debug, "Starting a new Geobot supervisor ~s~n", [GeobotWorkerNameAtom]),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
init([GeobotID]) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 50},

    GeobotWorkerSpec = #{
        id => imersia_geobot_worker,
        start => {imersia_geobot_worker, start_link, [GeobotID]},
        type => worker,
        restart => permanent
    },

    AutomationSupervisorSpec = #{
        id => imersia_automations_sup,
        start => {imersia_automations_sup, start_link, [GeobotID]},
        type => supervisor,
        restart => permanent
    },

    AutomationHandlerSpec = #{
        id => imersia_automations_handler,
        start => {imersia_automations_handler, start_link, [GeobotID]},
        type => worker,
        restart => permanent
    },

    EntanglementHandlerSpec = #{
        id => imersia_entanglements_sup,
        start => {imersia_entanglements_sup, start_link, [GeobotID]},
        type => worker,
        restart => permanent
    },

    {ok, {RestartStrategy, [EntanglementHandlerSpec, AutomationHandlerSpec, AutomationSupervisorSpec, GeobotWorkerSpec]}}.
% ----------------------------------------------------------------------------------------------------
