% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The MRServer Supervisor
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(mrserver_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 20, period => 100},

    % Start the Cowboy Webserver for managing the API and WebSockets
    APISupervisorSpec = #{id => api_sup,
        start => {api_sup, start_link, []},
        type => supervisor,
        modules => []},

    % Wait a moment for the DB and web server to catch up
    timer:sleep(5000),

    % Start the Velocity Engine
    VelEngineSupervisorSpec = #{id => imersia_velocity_engine_sup,
        start => {imersia_velocity_engine_sup, start_link, []},
        type => supervisor,
        modules => []},

    % Start the License Supervisor
    LicenseSupervisorSpec = #{id => imersia_license_sup,
        start => {imersia_license_sup, start_link, []},
        type => supervisor,
        modules => []},

    {ok, {RestartStrategy, [APISupervisorSpec, VelEngineSupervisorSpec, LicenseSupervisorSpec]}}.
