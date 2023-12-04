% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Websocket Companion Handler
% ----------------------------------------------------------------------------------------------------
-module(imersia_companion_socket).

-define(TIME_OUT, 60000).
-define(PING_TIME, 40000).

-include("../imersia_datatypes.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

% ----------------------------------------------------------------------------------------------------
% Specify this as a Websocket
% ----------------------------------------------------------------------------------------------------
init(Req, _) ->
    UserID = cowboy_req:binding(userid, Req),
    State = {ready, #{userid => UserID, sessionid => undefined}},

    Opts = #{compress => true, idle_timeout => ?TIME_OUT},
    {cowboy_websocket, Req, State, Opts}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Initialise this instance of Websocket
% ----------------------------------------------------------------------------------------------------
websocket_init(State) ->
    {ready, #{userid := UserID}} = State,
    % Start the pinger to keep the connection alive
    erlang:start_timer(?PING_TIME, self(), <<"ping">>),
    % Inform the Companion that this socket is open
    if
        UserID == undefined ->
            {ok, State};
        true ->
            % Set up a connection to your Companion Process
            UserIDAtom = binary_to_atom(UserID, utf8),
            case whereis(UserIDAtom) of
                undefined -> {ok, State};
                _ ->
                    UserIDAtom ! {listen, self()},
                    {ok, State}
            end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Handle messages coming from the client
% Message Format: {command: Command, sessionid: SessionID, parameters: Parameters}
% ----------------------------------------------------------------------------------------------------
websocket_handle({text, <<"pong">>}, State) -> {ok, State};
websocket_handle({text, <<"Connected">>}, State) -> {ok, State};
websocket_handle({text, <<"connected">>}, State) -> {ok, State};
websocket_handle({text, Msg}, {ready, StateParameters}) ->
    #{userid := UserID, sessionid := SessionIDIn} = StateParameters,

    MsgJSON = imersia_misc:safely_decode_json(Msg),

    Command = ej:get({"command"}, MsgJSON),

    SessionID = test_sessionid(SessionIDIn, ej:get({"sessionid"}, MsgJSON), UserID),
    CommandParameters = ej:get({"parameters"}, MsgJSON),

    interpret_command(Command, SessionID, CommandParameters, {ready, StateParameters#{sessionid := SessionID}});

websocket_handle(_Data, State) ->
	{ok, State}.

test_sessionid(undefined, undefined, _) -> undefined;
test_sessionid(undefined, SessionID, UserID) ->
    Connection = imersia_db:connect(),
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> SessionID;
        {ok, _} -> undefined;
        {error, _} -> undefined
    end;
test_sessionid(SessionID, SessionID, _) -> SessionID;
test_sessionid(_, _, _) -> undefined.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Handle messages from outside
% ----------------------------------------------------------------------------------------------------
% SessiondID is undefined, so send back an error
interpret_command(_, undefined, _, State) ->
    {reply, {text, jsx:encode([{<<"error">>, <<"sessionid">>}])}, State};

% No Command was sent, so send back an error
interpret_command(undefined, _, _, State) ->
    {reply, {text, jsx:encode([{<<"error">>, <<"command">>}])}, State};

% Someone sent a message
interpret_command(<<"message">>, _, Parameters, {ready, StateParameters}) ->
    #{userid := UserID} = StateParameters,
    ID = ej:get({"id"}, Parameters),
    case ID of
        undefined ->
            {reply, {text, jsx:encode([{<<"msg">>, <<"Hello there!">>}])}, {ready, StateParameters}};
        UserID ->
            {reply, {text, jsx:encode([{<<"msg">>, <<"Hello there!">>}])}, {ready, StateParameters}};
        EntityID ->
            EntityIDAtom = binary_to_atom(EntityID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
                _ ->
                    EntityIDAtom ! {message, self()},
                    {ok, {ready, StateParameters}}
            end
    end;

% An event from outside
interpret_command(<<"event">>, _, Parameters, {ready, StateParameters}) ->
    #{userid := UserID} = StateParameters,
    ID = ej:get({"id"}, Parameters),
    Event = ej:get({"event"}, Parameters),
    EventParameters = [{<<"userid">>, UserID} | check_event_parameters(ej:get({"parameters"}, Parameters))],
    case ID of
        undefined ->
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        UserID ->
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        EntityID ->
            EntityIDAtom = binary_to_atom(EntityID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
                _ ->
                    EntityIDAtom ! {event, {Event, EventParameters}},
                    {ok, {ready, StateParameters}}
            end
    end;

% Someone wants to wotch a Channel or Geobot
interpret_command(<<"wotcha">>, _, Parameters, {ready, StateParameters}) ->
    #{userid := UserID} = StateParameters,
    ID = ej:get({"id"}, Parameters),
    case ID of
        undefined ->
            % No ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        UserID ->
            % Is the User's own ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        EntityID ->
            EntityIDAtom = binary_to_atom(EntityID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
                _ ->
                    EntityIDAtom ! {listen, UserID},
                    {ok, {ready, StateParameters}}
            end
    end;

% Someone wants to stop wotching a Channel or Geobot
interpret_command(<<"close">>, _, Parameters, {ready, StateParameters}) ->
    #{userid := UserID} = StateParameters,
    ID = ej:get({"id"}, Parameters),
    case ID of
        undefined ->
            % No ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        UserID ->
            % Is the User's own ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        EntityID ->
            EntityIDAtom = binary_to_atom(EntityID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
                _ ->
                    EntityIDAtom ! {close, UserID},
                    {ok, {ready, StateParameters}}
            end
    end;

% Checking the status of automations
interpret_command(<<"status">>, _, Parameters, {ready, StateParameters}) ->
    #{userid := UserID} = StateParameters,
    ID = ej:get({"id"}, Parameters),
    case ID of
        undefined ->
            % No ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        UserID ->
            % Is the User's own ID, send back an error
            {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
        EntityID ->
            EntityIDAtom = binary_to_atom(EntityID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> {reply, {text, jsx:encode([{<<"error">>, <<"id">>}])}, {ready, StateParameters}};
                _ ->
                    EntityIDAtom ! {status},
                    {ok, {ready, StateParameters}}
            end
    end;

interpret_command(Message, _, _, State) ->
    % An unrecognized commend
    {reply, {text, jsx:encode([{<<"error">>, <<Message/binary, " is not recognized">>}])}, State}.


check_event_parameters(undefined) -> [];
check_event_parameters(Parameters) -> Parameters.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Handle messages from the Pinger, Companions, Channels and Geobots
% ----------------------------------------------------------------------------------------------------
websocket_info({timeout, _Ref, <<"ping">>}, State) ->
    erlang:start_timer(?PING_TIME, self(), <<"ping">>),
	{reply, {text, <<"ping">>}, State};

websocket_info({message, _Ref, Message}, State) ->
    {reply, {text, jsx:encode([{<<"message">>, Message}])}, State};

websocket_info({wotcha, _Ref, Message}, State) ->
    case filter_message(Message) of
        removed -> {ok, State};
        MessageToSend ->
            {reply, {text, jsx:encode([{<<"wotcha">>, MessageToSend}])}, State}
    end;

websocket_info(Info, State) ->
    imersia_misc:debug(error, "Unknown message at Companion : ~0p.~n", [Info]),
    {ok, State}.

filter_message([{<<"trigger">>, Message}]) -> [{<<"trigger">>, Message}];
filter_message(_) -> removed.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The connection closes, remove the link to the companion
% ----------------------------------------------------------------------------------------------------
terminate(_Reason, _Req, State) ->
    {ready, #{userid := UserID}} = State,
    % Inform the Companion that this socket is now closed
    if
        UserID == undefined ->
            ok;
        true ->
            % Remove the connection from your Companion Process
            UserIDAtom = binary_to_atom(UserID, utf8),
            case whereis(UserIDAtom) of
                undefined -> ok;
                _ ->
                    UserIDAtom ! {close, self()},
                    ok
            end
    end.
% ----------------------------------------------------------------------------------------------------
