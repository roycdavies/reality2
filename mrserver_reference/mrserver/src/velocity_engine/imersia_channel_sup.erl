% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Channel Supervisor for the Velocity Engine
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_channel_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/0]).
-export([init/1]).

-export([load_channels/0, load_channels/2, kill_channel/1, start_channel/1, notify_channel/2]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link() ->
    Result = supervisor:start_link({local, imersia_channel_sup}, imersia_channel_sup, []),

    DisplayResult = fun(Param) ->
        imersia_misc:debug(debug, "Channels Started: ~0p~n", [Param])
    end,
    load_channels(DisplayResult, 10),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => simple_one_for_one, intensity => 10, period => 50},

    ChildSpec = #{
        id => imersia_channel_worker,
        start => {imersia_channel_worker, start_link, []},
        type => worker,
        restart => permanent
    },

    {ok, {RestartStrategy, [ChildSpec]}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Loads the existing channels from the database and starts workers for each of them.
% If the Channels are already started, doesn't restart them.
% Registers names for each Channel Worker using the ChannelIDs
% ----------------------------------------------------------------------------------------------------
load_channels() -> load_channels(fun(Param) -> Param end, 10).
load_channels(_, 0) -> imersia_misc:debug(error, "Error in Channel Supervisor~n", []);
load_channels(Callback, Count) ->
    Connection = imersia_db:connect(),
    case imersia_db:channel_list(Connection) of
        % Get the list of channels
        {ok, ChannelList} -> Callback(load_channels(ChannelList));
        % If error, try again after 1 second
        {error, _} -> timer:apply_after(1000, imersia_channel_sup, load_channels, [Callback, Count-1])
    end.

load_channels([]) -> [];
load_channels([ChannelID | ChannelIDs]) ->
    case start_channel(ChannelID) of
        {ok, {ChannelID, Pid}} -> [{ChannelID, Pid} | load_channels(ChannelIDs)];
        {error, _} -> load_channels(ChannelIDs)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Start a Channel Worker given a ChannelID
% ----------------------------------------------------------------------------------------------------
start_channel(ChannelID) ->
    ChannelIDAtom = binary_to_atom(ChannelID, utf8),
    case whereis(ChannelIDAtom) of
        undefined ->
            Result = supervisor:start_child(imersia_channel_sup, [ChannelID]),
            case Result of
                {ok, Pid} -> {ok, {ChannelID, Pid}};
                _Error -> {error, start_child}
            end;
        Pid -> {ok, {ChannelID, Pid}}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send a message to a channel worker
% ----------------------------------------------------------------------------------------------------
notify_channel(ChannelID, Message) ->
    ChannelIDAtom = binary_to_atom(ChannelID, utf8),
    case whereis(ChannelIDAtom) of
        undefined -> ok;
        _ -> ChannelIDAtom ! Message
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Kill a specific channel given the ChannelID
% ----------------------------------------------------------------------------------------------------
kill_channel(ChannelID) ->
    ChannelIDAtom = binary_to_atom(ChannelID, utf8),
    imersia_misc:debug(debug, "Killing Channel ~s~n", [ChannelID]),
    case whereis(ChannelIDAtom) of
        undefined -> {ok, dead};
        Pid ->
            Result = supervisor:terminate_child(imersia_channel_sup, Pid),
            case Result of
                ok -> {ok, dead};
                {error, not_found} -> {ok, dead};
                _ -> {error, not_dead}
            end
    end.
% ----------------------------------------------------------------------------------------------------
