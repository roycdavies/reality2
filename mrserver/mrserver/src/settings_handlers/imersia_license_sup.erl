% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Supervisor for the license worker.
% ----------------------------------------------------------------------------------------------------
-module(imersia_license_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/0, init/1]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link() ->
    imersia_misc:debug(debug, "Starting License Checker~n", []),

    supervisor:start_link({local, license_sup}, imersia_license_sup, []).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 10, period => 50},
    ChildSpec = #{
        id => imersia_license_worker,
        start => {imersia_license_worker, start_link, []},
        type => worker,
        restart => permanent
    },
    {ok, {RestartStrategy, [ChildSpec]}}.
% ----------------------------------------------------------------------------------------------------
