% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Channel Worker
% ----------------------------------------------------------------------------------------------------

-module(imersia_geobot_worker).

-include("../imersia_datatypes.hrl").

-export([start_link/1, loop/2, terminate/2]).

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_link(GeobotID) ->
    GeobotIDAtom = binary_to_atom(GeobotID, utf8),
    Connection = imersia_db:connect(),
    Result = case imersia_db:geobot_getdetails(Connection, GeobotID) of
        {ok, Geobot} ->
            WotchaIDs = case application:get_env(mrserver, GeobotIDAtom) of
                {ok, StoredIDs} -> StoredIDs;
                _ -> []
            end,
            imersia_misc:debug(debug, "Starting a new Geobot worker ~s with ~p~n", [GeobotID, WotchaIDs]),
            GeobotPid = spawn_link(imersia_geobot_worker, loop, [Geobot, WotchaIDs]),
            register(GeobotIDAtom, GeobotPid),
            {ok, GeobotPid};
        _ ->
            imersia_misc:debug(error, "Error Starting Geobot Worker~n", []),
            {error}
    end,
    imersia_db:close(Connection),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
terminate(Event, _State) ->
    imersia_misc:debug(debug, "Terminating ~s~n", [Event]),
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The main loop - receives messages and processes them accordingly
% ----------------------------------------------------------------------------------------------------
loop(Details, Wotcha) ->
    receive
        % --------------------------------------------------------------------------------------------
        % Messages from internal workings
        % --------------------------------------------------------------------------------------------

        % When Geobot details change, this is called by the API or Velocity Engine.
        {update, NewDetails} ->
            send_message_to_each_companion([{<<"geobot_set">>, imersia_misc:record_to_json(NewDetails, false)}], Details, Wotcha),
            loop(NewDetails, Wotcha);

        % When Geobot metadata is altered in some way, this is called by the API or Velocity Engine.
        {metadata, Metadata} ->
            MetadataToSend = [{<<"geobotid">>, Details#geobot.geobotid} | imersia_misc:record_to_json(Metadata, false)],
            send_message_to_each_companion([{<<"geobot_metadata">>, MetadataToSend}], Details, Wotcha),
            % Notify the Channel worker of the change in Geobot metadata
            MetadataToSendToChannel = [{<<"channelid">>, Details#geobot.channelid} | MetadataToSend],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotmetadata, MetadataToSendToChannel}
            end,
            loop(Details, Wotcha);

        % When automations are altered
        {automationsrestart, Automations} ->
            imersia_automations_sup:update_automations(Details#geobot.geobotid, Automations),
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"automations">>, convert_automation_list(Automations)}],
            send_message_to_each_companion([{<<"geobot_automations">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker of the change in Geobot automations
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"automations">>, convert_automation_list(Automations)}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {automationsrestart, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % When metadata is deleted
        {metadatadelete, MetadataID} ->
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"metadataid">>, MetadataID}],
            send_message_to_each_companion([{<<"geobot_metadata_delete">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker of the change in Geobot metadata
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"metadataid">>, MetadataID}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotmetadatadelete, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % When Geobot context is altered in some way, this is called by the API or Velocity Engine.
        {context, Context} ->
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid} | imersia_misc:record_to_json(Context, false)],
            send_message_to_each_companion([{<<"geobot_context">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker of the change in Geobot metadata
            MessageToSendToChannel = [{<<"channelid">>, Details#geobot.channelid} | MessageToSend],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotcontext, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % When Geobot context is deleted, this is called by the API or velocity Engine.
        {contextdelete, ContextID} ->
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"contextid">>, ContextID}],
            send_message_to_each_companion([{<<"geobot_context_delete">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker of the change in Geobot metadata
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"contextid">>, ContextID}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotcontextdelete, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % When the Geobot is deleted
        {delete, GeobotID} ->
            MessageToSend = [{<<"channelid">>, Details#geobot.channelid}, {<<"geobotid">>, GeobotID}],
            send_message_to_each_companion([{<<"geobot_delete">>, MessageToSend}], Details, Wotcha),
            loop(Details, Wotcha);

        % When an Automation changes state
        {statechange, AutomationID, NewState, _ActionsResults} ->
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"automationid">>, AutomationID}, {<<"newstate">>, NewState}], %, {<<"results">>, ActionsResults}],
            send_message_to_each_companion([{<<"state_change">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"automationid">>, AutomationID}, {<<"newstate">>, NewState}], %, {<<"results">>, ActionsResults}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotstatechange, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % The return from a status update request from a user
        {status_return, AutomationStates} ->
            imersia_misc:debug_to_logfile(Details#geobot.geobotid, debug, "Status request received.~n", []),
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"states">>, AutomationStates}],
            send_message_to_each_companion([{<<"geobot_status">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"states">>, AutomationStates}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {geobotstatus_return, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % A Trigger event coming from the Velocity Engine
        {trigger, Event, Parameters} ->
            MessageToSend = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"event">>, Event}, {<<"parameters">>, Parameters}],
            send_message_to_each_companion([{<<"trigger">>, MessageToSend}], Details, Wotcha),
            % Notify the Channel worker
            MessageToSendToChannel = [{<<"geobotid">>, Details#geobot.geobotid}, {<<"channelid">>, Details#geobot.channelid}, {<<"event">>, Event}, {<<"parameters">>, Parameters}],

            ChannelIDAtom = binary_to_atom(Details#geobot.channelid, utf8),
            case whereis(ChannelIDAtom) of
                undefined -> ok;
                _ -> ChannelIDAtom ! {trigger, MessageToSendToChannel}
            end,
            loop(Details, Wotcha);

        % --------------------------------------------------------------------------------------------
        % Messages to send to Geobots and Automations
        % --------------------------------------------------------------------------------------------

        % Add a Companion (ie UserID) to the list of wotching processes if not already on the list
        {listen, UserID} ->
            case lists:member(UserID, Wotcha) of
                true -> loop(Details, Wotcha);
                false ->
                    case check_is_owner_or_public(UserID, Details) of
                        true ->
                            imersia_misc:debug_to_logfile(Details#geobot.geobotid, debug, "~s is listening to Geobot~n", [UserID]),
                            % Save the list of IDs in case the thread crashes and we have to reconstruct.
                            NewWotchas = [UserID | Wotcha],
                            GeobotIDAtom = binary_to_atom(Details#geobot.geobotid, utf8),
                            application:set_env(mrserver, GeobotIDAtom, NewWotchas, [{persistent, true}]),
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
                    imersia_misc:debug_to_logfile(Details#geobot.geobotid, debug, "~s has stopped listening to Geobot~n", [UserID]),
                    % Save the list of IDs in case the thread crashes and we have to reconstruct.
                    NewWotchas = lists:delete(UserID, Wotcha),
                    GeobotIDAtom = binary_to_atom(Details#geobot.geobotid, utf8),
                    application:set_env(mrserver, GeobotIDAtom, NewWotchas, [{persistent, true}]),
                    loop(Details, NewWotchas)
            end;

        % A general message has come in.
        {message, Pid} ->
            GeobotID = Details#geobot.geobotid,
            % Send message back to process that sent message
            Pid ! {message, self(), [{geobotid, GeobotID}, {message, <<"hello">>}]},
            loop(Details, Wotcha);

        % Send Event to all Automations attached to this Geobot
        {event, {Event, Parameters}} ->
            GeobotID = Details#geobot.geobotid,
            GeobotAutoSupNameAtom = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
            case whereis(GeobotAutoSupNameAtom) of
                undefined -> imersia_misc:debug_to_logfile(GeobotID, error, "Automation supervisor seems to be dead.~n", []);
                _ ->
                    imersia_misc:debug_to_logfile(GeobotID, debug, "Event [~s] with Parameters (~p) received and passed to automation supervisor [~s].~n", [Event, Parameters, GeobotAutoSupNameAtom]),
                    GeobotAutoSupNameAtom ! {Event, Parameters}
            end,
            loop(Details, Wotcha);

        % Request a status update from the Automations
        {status} ->
            GeobotID = Details#geobot.geobotid,
            GeobotAutoSupNameAtom = binary_to_atom(<< GeobotID/binary, "_auto" >>, utf8),
            case whereis(GeobotAutoSupNameAtom) of
                undefined -> imersia_misc:debug_to_logfile(GeobotID, error, "Automation supervisor seems to be dead.~n", []);
                _ ->
                    GeobotAutoSupNameAtom ! {status}
            end,
            loop(Details, Wotcha);

        {die} -> exit(self(), err);

        Message ->
            imersia_misc:debug_to_logfile(Details#geobot.geobotid, error, "Unknown message at Geobot worker ~0p.~n", [Message]),
            loop(Details, Wotcha)
    end,
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check that the geobot is owner by the user, or is not hidden
% ----------------------------------------------------------------------------------------------------
check_is_owner_or_public(UserID, Details) ->
    if
        ((UserID == Details#geobot.ownerid) or (not Details#geobot.hidden)) -> true;
        true -> false
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
convert_automation_list([]) -> [];
convert_automation_list([Automation | Automations]) ->
    [imersia_misc:record_to_json(Automation, false) | convert_automation_list(Automations)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send some data to each Companion that is wotching to send on to any sockets that might be connected
% ----------------------------------------------------------------------------------------------------
send_message_to_each_companion(_, _, []) -> ok;
send_message_to_each_companion(Message, Details, [CompanionID | MoreIDs]) ->
    %% Double check public access or ownership incase it has changed after being wotched.
    case check_is_owner_or_public(CompanionID, Details) of
        true ->
            CompanionIDAtom = binary_to_atom(CompanionID, utf8),
            case whereis(CompanionIDAtom) of
                undefined -> ok;
                _ ->
                    CompanionIDAtom ! {wotcha, Message}
            end,
            send_message_to_each_companion(Message, Details, MoreIDs);
        false ->
            send_message_to_each_companion(Message, Details, MoreIDs)
    end.
% ----------------------------------------------------------------------------------------------------
