% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Geobot Supervisor for the Velocity Engine
% ----------------------------------------------------------------------------------------------------


%% @hidden

-module(imersia_geobot_sup_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/0]).
-export([init/1]).

-export([load_geobots/0, load_geobots/2, kill_geobot/1, start_geobot/1, notify_geobot/2]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link() ->
    Result = supervisor:start_link({local, imersia_geobot_sup_sup}, imersia_geobot_sup_sup, []),

    DisplayResult = fun(Param) ->
        imersia_misc:debug(debug, "Geobots Started : ~0p~n", [Param])
    end,
    load_geobots(DisplayResult, 10),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
process_name(GeobotID) -> binary_to_atom(<< GeobotID/binary, "_sup" >>, utf8).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => simple_one_for_one, intensity => 20, period => 100},

    GeobotWorkerSpec = #{
        id => imersia_geobot_sup,
        start => {imersia_geobot_sup, start_link, []},
        type => supervisor,
        restart => permanent,
        modules => []
    },

    {ok, {RestartStrategy, [GeobotWorkerSpec]}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Loads the existing Geobots from the database and starts workers for each of them.
% If the Geobots are already started, doesn't restart them.
% Registers names for each Geobot Worker using the GeobotIDs
% ----------------------------------------------------------------------------------------------------
load_geobots() -> load_geobots(fun(Param) -> Param end, 10).
load_geobots(_, 0) -> imersia_misc:debug(error, "Error in Geobot Supervisor~n", []);
load_geobots(Callback, Count) ->
    Connection = imersia_db:connect(),
    case imersia_db:geobot_list(Connection) of
        % Get the list of geobots
        {ok, GeobotList} -> Callback(load_geobots(GeobotList));
        % If error, try again after 1 second
        {error, _} -> timer:apply_after(1000, imersia_geobot_sup_sup, load_geobots, [Callback, Count-1])
    end.

load_geobots([]) -> [];
load_geobots([GeobotID | GeobotIDs]) ->
    case start_geobot(GeobotID) of
        {ok, {GeobotID, Pid}} -> [{GeobotID, Pid} | load_geobots(GeobotIDs)];
        {error, _} -> load_geobots(GeobotIDs)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Start a Geobot Worker and its Automations given a Geobot Record
% ----------------------------------------------------------------------------------------------------
start_geobot(GeobotID) ->
    GeobotIDAtom = process_name(GeobotID),
    case whereis(GeobotIDAtom) of
        undefined ->
            Result = supervisor:start_child(imersia_geobot_sup_sup, [GeobotID]),
            case Result of
                {ok, Pid} -> {ok, {GeobotID, Pid}};
                _Error -> {error, start_child}
            end;
        Pid -> {ok, {GeobotID, Pid}}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send a message to a Geobot worker
% ----------------------------------------------------------------------------------------------------
notify_geobot(GeobotID, Message) ->
    GeobotIDAtom = binary_to_atom(GeobotID, utf8),
    case whereis(GeobotIDAtom) of
        undefined -> imersia_misc:debug(error, "Error notifying Geobot [~s]~n", [GeobotID]), ok;
        _ -> GeobotIDAtom ! Message
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Kill a specific Geobot given the GeobotID
% ----------------------------------------------------------------------------------------------------
kill_geobot(GeobotID) ->
    GeobotIDAtom = process_name(GeobotID),
    imersia_misc:debug(debug, "Killing Geobot ~s~n", [GeobotIDAtom]),
    case whereis(GeobotIDAtom) of
        undefined -> {ok, dead};
        Pid ->
            Result = supervisor:terminate_child(imersia_geobot_sup_sup, Pid),
            case Result of
                ok -> {ok, dead};
                {error, not_found} -> {ok, dead};
                _ -> {error, not_dead}
            end
    end.
% ----------------------------------------------------------------------------------------------------
