% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Channel Worker
% ----------------------------------------------------------------------------------------------------

-module(imersia_channel_worker).

-include("../imersia_datatypes.hrl").


-export([start_link/1, loop/2]).

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_link(ChannelID) ->
    ChannelIDAtom = binary_to_atom(ChannelID, utf8),
    Connection = imersia_db:connect(),
    Result = case imersia_db:channel_getdetails(Connection, ChannelID) of
        {ok, Channel} ->
            WotchaIDs = case application:get_env(mrserver, ChannelIDAtom) of
                {ok, StoredIDs} -> StoredIDs;
                _ -> []
            end,
            imersia_misc:debug(debug, "Starting a new Channel worker ~s with ~p~n", [ChannelID, WotchaIDs]),
            ChannelLoopPid = spawn_link(imersia_channel_worker, loop, [Channel, WotchaIDs]),
            register(ChannelIDAtom, ChannelLoopPid),
            {ok, ChannelLoopPid};
        _ ->
            imersia_misc:debug(error, "Error Starting Channel Worker~n", []),
            {error}
    end,
    imersia_db:close(Connection),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The main loop - receives messages and processes them accordingly
% ----------------------------------------------------------------------------------------------------
loop(Details, Wotcha) ->
    receive
        % When channel details change, this is called by the API or Velocity Engine.
        {update, NewDetails} ->
            send_message_to_each_companion([{<<"channel_set">>, imersia_misc:record_to_json(NewDetails, false)}], Details, Wotcha),
            loop(NewDetails, Wotcha);

        % When a channel is renamed, this is called by the API or Velocity Engine.
        {rename, NewName} ->
            NewDetails = Details#channel{name=NewName},
            send_message_to_each_companion([{<<"channel_rename">>, imersia_misc:record_to_json(NewDetails, false)}], Details, Wotcha),
            loop(NewDetails, Wotcha);

        {delete, _ChannelID} ->
            MetadataToSend = [{<<"channelid">>, Details#channel.channelid}],
            send_message_to_each_companion([{<<"channel_delete">>, MetadataToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When channel metadata is altered in some way, this is called by the API or Velocity Engine.
        {metadata, Metadata} ->
            MetadataToSend = [{<<"channelid">>, Details#channel.channelid} | imersia_misc:record_to_json(Metadata, false)],
            send_message_to_each_companion([{<<"channel_metadata">>, MetadataToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When geobot metadata is altered in some way, this is called by the API or Velocity Engine.
        {geobotmetadata, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_metadata">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When automations are altered
        {automationsrestart, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_automations">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When Channel metadata is deleted, this is called by the API or velocity Engine.
        {metadatadelete, MetadataID} ->
            MessageToSend = [{<<"channelid">>, Details#channel.channelid}, {<<"metadataid">>, MetadataID}],
            send_message_to_each_companion([{<<"channel_metadata_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When channel context is altered in some way, this is called by the API or Velocity Engine.
        {context, Context} ->
            MessageToSend = [{<<"channelid">>, Details#channel.channelid} | imersia_misc:record_to_json(Context, false)],
            send_message_to_each_companion([{<<"channel_context">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When Channel context is deleted, this is called by the API or velocity Engine.
        {contextdelete, ContextID} ->
            MessageToSend = [{<<"channelid">>, Details#channel.channelid}, {<<"contextid">>, ContextID}],
            send_message_to_each_companion([{<<"channel_context_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When geobot context is altered in some way, this is called by the API or Velocity Engine.
        {geobotcontext, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_context">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When Geobot context is deleted, this is called by the API or velocity Engine.
        {geobotcontextdelete, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_context_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When Geobot metadata is deleted, this is called by the API or velocity Engine.
        {geobotmetadatadelete, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_metadata_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When a Geobot on the Channel changes, this is called by the API or Velocity Engine.
        {geobotupdate, GeobotDetails} ->
            send_message_to_each_companion([{<<"geobot_set">>, imersia_misc:record_to_json(GeobotDetails, false)}], Details, Wotcha),
            loop(Details, Wotcha);

        {geobotnew, GeobotDetails} ->
            send_message_to_each_companion([{<<"geobot_new">>, imersia_misc:record_to_json(GeobotDetails, false)}], Details, Wotcha),
            loop(Details, Wotcha);

        {geobotdelete, GeobotID} ->
            MessageToSend = [{<<"channelid">>, Details#channel.channelid}, {<<"geobotid">>, GeobotID}],
            send_message_to_each_companion([{<<"geobot_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        {geobotstatechange, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_state_change">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        {geobotstatus_return, MessageToSend} ->
            send_message_to_each_companion([{<<"geobot_status">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        {trigger, MessageToSend} ->
            send_message_to_each_companion([{<<"trigger">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % Add a Companion (ie UserID) to the list of wotching processes if not already in the list
        {listen, UserID} ->
            case lists:member(UserID, Wotcha) of
                true -> loop(Details, Wotcha);
                false ->
                    case check_is_owner_or_public(UserID, Details) of
                        true ->
                            imersia_misc:debug_to_logfile(Details#channel.channelid, debug, "~s is listening to Channel~n", [UserID]),
                            % Save the list of IDs in case the thread crashes and we have to reconstruct.
                            NewWotchas = [UserID | Wotcha],
                            ChannelIDAtom = binary_to_atom(Details#channel.channelid, utf8),
                            application:set_env(mrserver, ChannelIDAtom, NewWotchas, [{persistent, true}]),
                            loop(Details, NewWotchas);
                        false ->
                            loop(Details, Wotcha)
                    end
            end;

        % Remove a Companion from the list of wotching processes
        {close, UserID} ->
            case lists:member(UserID, Wotcha) of
                false -> loop(Details, Wotcha);
                true ->
                    imersia_misc:debug_to_logfile(Details#channel.channelid, debug, "~s has stopped listening to Channel~n", [UserID]),
                    % Save the list of IDs in case the thread crashes and we have to reconstruct.
                    NewWotchas = lists:delete(UserID, Wotcha),
                    ChannelIDAtom = binary_to_atom(Details#channel.channelid, utf8),
                    application:set_env(mrserver, ChannelIDAtom, NewWotchas, [{persistent, true}]),
                    loop(Details, NewWotchas)
            end;

        {message, Pid} ->
            ChannelID = Details#channel.channelid,
            Pid ! {message, self(), [{channelid, ChannelID}, {message, <<"hello">>}]},
            loop(Details, Wotcha);

        {die} -> exit(self(), err);

        Message ->
            imersia_misc:debug_to_logfile(Details#channel.channelid, error, "Unknown message at Channel: ~0p~n", [Message]),
            loop(Details, Wotcha)
    end,
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Chech that the user id is either of the owner of the channel, or that the channle is not hidden
% ----------------------------------------------------------------------------------------------------
check_is_owner_or_public(UserID, Details) ->
    if
        ((UserID == Details#channel.ownerid) or (not Details#channel.hidden)) -> true;
        true -> false
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send some data to each Companion that is wotching to send on to any sockets that might be connected
% ----------------------------------------------------------------------------------------------------
send_message_to_each_companion(_, _, []) -> ok;
send_message_to_each_companion(Message, Details, [CompanionID | MoreIDs]) ->
    % Double check in case channel became hidden after it was wotched.
    case check_is_owner_or_public(CompanionID, Details) of
        true ->
            CompanionIDAtom = binary_to_atom(CompanionID, utf8),
            case whereis(CompanionIDAtom) of
                undefined -> ok;
                _ ->
                    imersia_misc:debug_to_logfile(CompanionID, debug, "Sending message to Companion~n", []),
                    CompanionIDAtom ! {wotcha, Message}
            end,
            send_message_to_each_companion(Message, Details, MoreIDs);
        false ->
            send_message_to_each_companion(Message, Details, MoreIDs)
    end.
% ----------------------------------------------------------------------------------------------------
