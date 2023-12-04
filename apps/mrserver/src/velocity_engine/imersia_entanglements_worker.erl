% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Process to manage the connection with another MRServer
% ----------------------------------------------------------------------------------------------------
-module(imersia_entanglements_worker).

-include("../imersia_datatypes.hrl").

-export([start_link/1, loop/1]).



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_link(EntanglementDetails) ->
	#{id := EntanglementProcessAtom} = EntanglementDetails,
    imersia_misc:debug(debug, "Starting a new Entanglement worker ~p~n", [EntanglementProcessAtom]),
    EnganglementPid = spawn_link(imersia_entanglements_worker, loop, [EntanglementDetails]),
    register(EntanglementProcessAtom, EnganglementPid),
	EnganglementPid ! {start},
	ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The main loop - receives messages and processes them accordingly
% ----------------------------------------------------------------------------------------------------
loop(EntanglementDetails) ->
    receive
		{start} ->
			NewEntanglementDetails = EntanglementDetails#{state => getting_sessionid},
			get_sessionid(NewEntanglementDetails),
			loop(NewEntanglementDetails);
		{disentangle} ->
			% Close the websocket and send an event to that effect.
			#{connpid := ConnPid, mrserver := MRServer, geobotid := GeobotID, remotegeobotid := RemoteGeobotID} = EntanglementDetails,
			gun:close(ConnPid),
			imersia_misc:debug_to_logfile(GeobotID, debug, "Geobot disentangled from ~s@~s~n", [RemoteGeobotID, MRServer]),
			send_message_to_local_geobot(GeobotID, <<"disentangled">>, [{<<"url">>, MRServer}, {<<"geobotid">>, RemoteGeobotID}]);
        {sessionid, SessionID} ->
			NewEntanglementDetails = EntanglementDetails#{sessionid => SessionID, state => getting_userid},
			get_userid(NewEntanglementDetails),
            loop(NewEntanglementDetails);
		{userid, UserID} ->
			NewEntanglementDetails = EntanglementDetails#{userid => UserID, state => opening_websocket},
			upgrade_websocket(NewEntanglementDetails),
            loop(NewEntanglementDetails);
		{event, {Event, Parameters}} ->
			#{geobotid := GeobotID} = EntanglementDetails,
			imersia_misc:debug_to_logfile(GeobotID, debug, "Sending event to remote Geobot~n", []),
			send_event(EntanglementDetails, Event, Parameters),
			loop(EntanglementDetails);
		{entangled} ->
			% Websocket open and geobot being wotched
			#{mrserver := MRServer, geobotid := GeobotID, remotegeobotid := RemoteGeobotID} = EntanglementDetails,
			imersia_misc:debug_to_logfile(GeobotID, debug, "Geobot entangled with ~s@~s~n", [RemoteGeobotID, MRServer]),
			send_message_to_local_geobot(GeobotID, <<"entangled">>, [{<<"url">>, MRServer}, {<<"geobotid">>, RemoteGeobotID}]),
			loop(EntanglementDetails);
		{websocket, _StreamRef} ->
			loop(EntanglementDetails);

		{gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
			% Websocket connected successfully
			NewEntanglementDetails = EntanglementDetails#{streamref => StreamRef, connpid => ConnPid, state => connected, counter => 0},
			send_wotcha(NewEntanglementDetails),
			loop(NewEntanglementDetails);
		{gun_down, ConnPid, ws, closed, _, _} ->
			% Websocket connection lost - try to re-establish link
			gun:close(ConnPid),
			NewEntanglementDetails = deal_with_error(EntanglementDetails#{counter => 0}, <<"Websocket closed">>),
			loop(NewEntanglementDetails);
		{gun_ws, _, _, Message} ->
			% Websocket message received from other MRServer
			interpret_ws_message(EntanglementDetails, Message),
			loop(EntanglementDetails);

		{error, Reason} ->
			% An error occurred - decide what to do.
			#{geobotid := GeobotID} = EntanglementDetails,
			imersia_misc:debug_to_logfile(GeobotID, error, "Entanglement error ~w~n", [Reason]),
			NewEntanglementDetails = case maps:is_key(counter, EntanglementDetails) of
				true ->
					deal_with_error(EntanglementDetails, Reason);
				false ->
					deal_with_error(EntanglementDetails#{counter => 0}, Reason)
			end,
			loop(NewEntanglementDetails);
		Message ->
			% Some other unhandled message
			#{geobotid := GeobotID} = EntanglementDetails,
			imersia_misc:debug_to_logfile(GeobotID, error, "Error ~w~n", [Message]),
			loop(EntanglementDetails)
	end,
	ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the sessionID from the remote server
% ----------------------------------------------------------------------------------------------------
get_sessionid(EntanglementDetails) ->
	#{id := EntanglementProcessAtom, mrserver := MRServer, useremail := UserEmail, password := Password, developerid := DeveloperID, geohash := GeoHash} = EntanglementDetails,

	case imersia_remote:get_sessionid(MRServer, DeveloperID, UserEmail, Password, GeoHash) of
		{ok, SessionID} ->
			EntanglementProcessAtom ! {sessionid, SessionID};
		{error, Reason} ->
			EntanglementProcessAtom ! {error, Reason}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the userID of the logged in user from the remote server
% ----------------------------------------------------------------------------------------------------
get_userid(EntanglementDetails) ->
	#{id := EntanglementProcessAtom, mrserver := MRServer, useremail := UserEmail, developerid := DeveloperID, geohash := GeoHash, sessionid := SessionID} = EntanglementDetails,

	case imersia_remote:get_userid(MRServer, DeveloperID, UserEmail, SessionID, GeoHash) of
		{ok, UserID} ->
			EntanglementProcessAtom ! {userid, UserID};
		{error, Reason} ->
			EntanglementProcessAtom ! {error, Reason}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Upgrade the connection to a websocket
% ----------------------------------------------------------------------------------------------------
upgrade_websocket(EntanglementDetails) ->
	#{id := EntanglementProcessAtom, mrserver := MRServer, userid := UserID, developerid := DeveloperID, geohash := GeoHash, sessionid := SessionID} = EntanglementDetails,

	case imersia_remote:connect_websocket(MRServer, DeveloperID, SessionID, UserID, GeoHash) of
		{ok, StreamRef} ->
			EntanglementProcessAtom ! {websocket, StreamRef};
		{error, Reason} ->
			EntanglementProcessAtom ! {error, Reason}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Make sure the companion on that webserver is wotching the desired Geobot
% ----------------------------------------------------------------------------------------------------
send_wotcha(EntanglementDetails) ->
	#{id := EntanglementProcessAtom, connpid := ConnPid, sessionid := SessionID, remotegeobotid := RemoteGeobotID} = EntanglementDetails,

	Message = [
		{<<"command">>, <<"wotcha">>},
		{<<"sessionid">>, SessionID},
		{<<"parameters">>, [
			{<<"id">>, RemoteGeobotID}
		]}
	],
	MessageString = imersia_misc:safely_encode_json(Message),

	gun:ws_send(ConnPid, {text, MessageString}),
	EntanglementProcessAtom ! {entangled}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send event to entangled Geobot
% ----------------------------------------------------------------------------------------------------
send_event(EntanglementDetails, Event, Parameters) ->
	#{connpid := ConnPid, sessionid := SessionID, remotegeobotid := RemoteGeobotID} = EntanglementDetails,

	Message = [
		{<<"command">>, <<"event">>},
		{<<"sessionid">>, SessionID},
		{<<"parameters">>, [
			{<<"id">>, RemoteGeobotID},
			{<<"event">>, Event},
			{<<"parameters">>, Parameters}
		]}
	],
	MessageString = imersia_misc:safely_encode_json(Message),

	gun:ws_send(ConnPid, {text, MessageString}).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Respond to a ping, and interpret anything else appropriately.
% ----------------------------------------------------------------------------------------------------
interpret_ws_message(EntanglementDetails, Message) ->
	#{connpid := ConnPid} = EntanglementDetails,
	case Message of
		{text, <<"ping">>} ->
			gun:ws_send(ConnPid, {text, <<"pong">>});
		{text, RawJSON} ->
			MessageJSON = imersia_misc:safely_decode_json(RawJSON),
			interpret_message_contents(EntanglementDetails, MessageJSON);
		Message ->
			imersia_misc:debug(debug, "Uninterpreted websocket Message ~w~n", [Message])
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Interpret the message contents coming from the websocket, checking that it is coming frome the correct
% Geobot, and then converting the trigger into an event for the local Geobot.
% Further, we only want the event that was sent to the Geobot, not the Channel (in case the Companion is also wotching the channel).
% ----------------------------------------------------------------------------------------------------
interpret_message_contents(EntanglementDetails, [{<<"error">>,<<"id">>}]) ->
	#{id := EntanglementProcessAtom} = EntanglementDetails,
	EntanglementProcessAtom ! {error, <<"Error connecting to Geobot">>};

interpret_message_contents(EntanglementDetails, MessageJSON) ->
	#{geobotid := GeobotID, remotegeobotid := RemoteGeobotID} = EntanglementDetails,
	% Make sure it is a wotcha event
	case ej:get({<<"wotcha">>}, MessageJSON) of
		undefined -> ok;
		WotchaJSON ->
			% and a trigger
			case ej:get({<<"trigger">>}, WotchaJSON) of
				undefined -> ok;
				TriggerJSON ->
					% from the geobot, not the channel
					case ej:get({<<"channelid">>}, TriggerJSON) of
						undefined ->
							% and it matches the geobotid we are waiting for
							case ej:get({<<"geobotid">>}, TriggerJSON) of
								RemoteGeobotID ->
									Event = ej:get({<<"event">>}, TriggerJSON),
									EventParameters = ej:get({<<"parameters">>}, TriggerJSON),
									imersia_misc:debug_to_logfile(GeobotID, debug, "Sending Event ~s with Parameters ~w~n", [Event, EventParameters]),
									send_message_to_local_geobot(GeobotID, Event, EventParameters);
								_ -> ok
							end;
						_ -> ok
					end
			end
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
send_message_to_local_geobot(GeobotID, Event, EventParameters) ->
	GeobotIDAtom = binary_to_atom(GeobotID, utf8),
	GeobotIDAtom ! {event, {Event, EventParameters}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Attempt to recover the connection if an error occurs.
% ----------------------------------------------------------------------------------------------------
deal_with_error(EntanglementDetails, _ErrorReason) ->
	#{state := State, id := EntanglementProcessAtom, geobotid := GeobotID, mrserver := MRServer, remotegeobotid := RemoteGeobotID, counter := Counter} = EntanglementDetails,
	case State of
		getting_sessionid ->
			% Wait a while and try again
			if
				Counter < 10 ->
					timer:send_after(1000, EntanglementProcessAtom, {start}); % every second
				Counter < 20 ->
					timer:send_after(10000, EntanglementProcessAtom, {start}); % every ten seconds
				Counter < 60 ->
					timer:send_after(60000, EntanglementProcessAtom, {start}); % every minute
				true ->
					timer:send_after(300000, EntanglementProcessAtom, {start}) % every 5 minutes
			end,
			EntanglementDetails#{counter => Counter+1};
		getting_userid ->
			% Wait a while and try again
			timer:send_after(10000, EntanglementProcessAtom, {start}),
			EntanglementDetails#{counter => Counter+1};
		connected ->
			send_message_to_local_geobot(GeobotID, <<"entangle_error">>, [{<<"url">>, MRServer}, {<<"geobotid">>, RemoteGeobotID}]),
			timer:send_after(10000, EntanglementProcessAtom, {start}),
			EntanglementDetails#{counter => Counter+1};
		_ -> EntanglementDetails#{counter => Counter+1}
	end.
% ----------------------------------------------------------------------------------------------------
