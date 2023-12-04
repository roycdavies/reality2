% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Companion Supervisor for the Velocity Engine
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_companion_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/0]).
-export([init/1]).

-export([load_users/0, load_users/2, kill_companion/1, start_companion/1, notify_companion/2]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link() ->
    Result = supervisor:start_link({local, imersia_companion_sup}, imersia_companion_sup, []),

    DisplayResult = fun(Param) ->
        imersia_misc:debug(debug, "Companions Started: ~0p~n", [Param])
    end,
    load_users(DisplayResult, 10),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => simple_one_for_one, intensity => 10, period => 50},

    ChildSpec = #{
        id => imersia_companion_worker,
        start => {imersia_companion_worker, start_link, []},
        type => worker,
        restart => permanent
    },

    {ok, {RestartStrategy, [ChildSpec]}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Loads the existing users from the database and starts workers for each of them.
% If the Users / Companions are already started, doesn't restart them.
% Registers names for each User Worker using the UserIDs
% ----------------------------------------------------------------------------------------------------
load_users() -> load_users(fun(Param) -> Param end, 10).
load_users(_, 0) -> imersia_misc:debug(error, "Error in Companion Supervisor~n", []);
load_users(Callback, Count) ->
    Connection = imersia_db:connect(),
    case imersia_db:user_list(Connection) of
        % Get the list of users
        {ok, UserList} -> Callback(load_users(UserList));
        % If error, try again after 1 second
        {error, _} -> timer:apply_after(1000, imersia_companion_sup, load_users, [Callback, Count-1])
    end.

load_users([]) -> [];
load_users([UserID | UserIDs]) ->
    case start_companion(UserID) of
        {ok, {UserID, Pid}} -> [{UserID, Pid} | load_users(UserIDs)];
        {error, _} -> load_users(UserIDs)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send a message to a companion worker
% ----------------------------------------------------------------------------------------------------
notify_companion(UserID, Message) ->
    UserIDAtom = binary_to_atom(UserID, utf8),
    case whereis(UserIDAtom) of
        undefined -> ok;
        _ -> UserIDAtom ! Message
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Start a Companion Worker given a User Record
% ----------------------------------------------------------------------------------------------------
start_companion(UserID) ->
    UserIDAtom = binary_to_atom(UserID, utf8),
    case whereis(UserIDAtom) of
        undefined ->
            Result = supervisor:start_child(imersia_companion_sup, [UserID]),
            case Result of
                {ok, Pid} -> {ok, {UserID, Pid}};
                _Error -> {error, start_child}
            end;
        Pid -> {ok, {UserID, Pid}}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Kill a specific Companion given the CompanionID
% ----------------------------------------------------------------------------------------------------
kill_companion(UserID) ->
    UserIDAtom = binary_to_atom(UserID, utf8),
    imersia_misc:debug(debug, "Killing Companion ~s~n", [UserID]),
    case whereis(UserIDAtom) of
        undefined -> {ok, dead};
        Pid ->
            Result = supervisor:terminate_child(imersia_companion_sup, Pid),
            case Result of
                ok -> {ok, dead};
                {error, not_found} -> {ok, dead};
                _ -> {error, not_dead}
            end
    end.
% ----------------------------------------------------------------------------------------------------
