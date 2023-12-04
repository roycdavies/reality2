% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Velocity Engine Supervisor
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_velocity_engine_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    imersia_misc:debug(debug, "Velocity Engine Starting~n", []),
    supervisor:start_link({local, imersia_velocity_engine_sup}, imersia_velocity_engine_sup, []).

init(_) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 50},

    CompanionSupervisorSpec = #{
        id => imersia_companion_sup,
        start => {imersia_companion_sup, start_link, []},
        type => supervisor,
        restart => permanent,
        modules => []
    },

    ChannelSupervisorSpec = #{
        id => imersia_channel_sup,
        start => {imersia_channel_sup, start_link, []},
        type => supervisor,
        restart => permanent,
        modules => []
    },

    GeobotSupervisorSupervisorSpec = #{
        id => imersia_geobot_sup_sup,
        start => {imersia_geobot_sup_sup, start_link, []},
        type => supervisor,
        restart => permanent,
        modules => []
    },

    {ok, {RestartStrategy, [CompanionSupervisorSpec, ChannelSupervisorSpec, GeobotSupervisorSupervisorSpec]}}.
