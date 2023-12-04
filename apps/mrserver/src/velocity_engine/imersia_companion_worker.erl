% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Companion Worker
% ----------------------------------------------------------------------------------------------------

-module(imersia_companion_worker).

-include("../imersia_datatypes.hrl").


-export([start_link/1, loop/2]).



% ----------------------------------------------------------------------------------------------------
% Start the main loop
% ----------------------------------------------------------------------------------------------------
start_link(UserID) ->
    UserIDAtom = binary_to_atom(UserID, utf8),
    Connection = imersia_db:connect(),
    Result = case imersia_db:user_get_details_by_userid(Connection, UserID) of
        {ok, User} ->
            Sockets = case application:get_env(mrserver, UserIDAtom) of
                {ok, StoredSockets} -> StoredSockets;
                _ -> []
            end,
            imersia_misc:debug(debug, "Starting a new Companion worker ~s with ~p~n", [UserID, Sockets]),
            CompanionLoopPid = spawn_link(imersia_companion_worker, loop, [User, Sockets]),
            register(UserIDAtom, CompanionLoopPid),
            {ok, CompanionLoopPid};
        _ ->
            imersia_misc:debug(error, "Error Starting Companion Worker~n", []),
            {error}
    end,
    imersia_db:close(Connection),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The main loop - receives messages and processes them accordingly
% ----------------------------------------------------------------------------------------------------
loop(Details, Sockets) ->
    receive
        % Companion details have been changed
        {update, NewDetails} ->
            send_message_to_sockets(Details#user.userid, [{<<"user_set">>, imersia_misc:record_to_json(NewDetails, false)}], Sockets),
            loop(NewDetails, Sockets);

        % Some tokens have been spent
        {tokens, NumTokens} ->
            MessageToSend = [{<<"userid">>, Details#user.userid}, {<<"tokens">>, NumTokens}],
            send_message_to_sockets(Details#user.userid, [{<<"user_tokens">>, MessageToSend}], Sockets),
            loop(Details, Sockets);

        % When companion metadata is altered in some way, this is called by the API or Velocity Engine.
        {metadata, Metadata} ->
            MetadataToSend = [{<<"userid">>, Details#user.userid} | imersia_misc:record_to_json(Metadata, false)],
            send_message_to_sockets(Details#user.userid, [{<<"user_metadata">>, MetadataToSend}], Sockets),
            loop(Details, Sockets);

        {metadatadelete, MetadataID} ->
            MessageToSend = [{<<"userid">>, Details#user.userid}, {<<"metadataid">>, MetadataID}],
            send_message_to_sockets(Details#user.userid, [{<<"user_metadata_delete">>, MessageToSend}], Sockets),
            loop(Details, Sockets);

        % When a new Channel is created, notify the companion wotchers
        {channelnew, ChannelDetails} ->
            send_message_to_sockets(Details#user.userid, [{<<"channel_new">>, imersia_misc:record_to_json(ChannelDetails, false)}], Sockets),
            loop(Details, Sockets);

        % A new WebSocket has been opened to listen to this channel
        {listen, NewSocket} when is_pid(NewSocket) ->
            imersia_misc:debug_to_logfile(Details#user.userid, debug, "A Socket is listening to the Companion~n", []),
            % Save the list of Sockets in case the thread crashes and we have to reconstruct.
            UserIDAtom = binary_to_atom(Details#user.userid, utf8),
            NewSockets = [NewSocket | Sockets],
            application:set_env(mrserver, UserIDAtom, NewSockets, [{persistent, true}]),
            loop(Details, NewSockets);
        % If is not a Pid, it might be someone trying to listen to another Companion, so ignore it.
        {listen, _} -> loop(Details, Sockets);

        % Remove a WebSocket from the list (ie when the socket closes)
        {close, OldSocket} ->
            imersia_misc:debug_to_logfile(Details#user.userid, debug, "A Socket has stopped listening to the Companion~n", []),
            loop(Details, lists:delete(OldSocket, Sockets));

        % Send a message to all the sockets
        {wotcha, Message} ->
            send_message_to_sockets(Details#user.userid, Message, Sockets),
            loop(Details, Sockets);

        % A simple Message response
        {message, Pid} ->
            UserID = Details#user.userid,
            Pid ! {message, self(), [{userid, UserID}, {message, <<"hello">>}]},
            loop(Details, Sockets);

        {die} -> exit(self(), err);

        Message ->
            imersia_misc:debug_to_logfile(Details#user.userid, error, "Unknown message at Companion Worker: ~0p.~n", [Message]),
            loop(Details, Sockets)
    end, ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
send_message_to_sockets(UserID, Message, Sockets) ->
    imersia_misc:debug_to_logfile(UserID, debug, "Sending message [~0p] to Sockets [~0p]~n", [Message, Sockets]),
    send_to_all_sockets(Message, Sockets).

send_to_all_sockets(_, []) -> ok;
send_to_all_sockets(Message, [Pid | MorePids]) ->
    Pid ! {wotcha, self(),  Message},
    send_to_all_sockets(Message, MorePids).
% ----------------------------------------------------------------------------------------------------
