% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Commands to set up and manage links with a Geobot on another MRServer
% ----------------------------------------------------------------------------------------------------
-module(imersia_remote).

-include("../imersia_datatypes.hrl").

-export([get_sessionid/5, get_userid/5, connect_websocket/5]).

% ----------------------------------------------------------------------------------------------------
% Connect to the specified MRServer (with URL) using the given UserEmail and Password, and return
% the sessionid: {ok, sessionid} or {error, Reason}
% ----------------------------------------------------------------------------------------------------
get_sessionid(undefined, _, _, _, _) -> {error, url};
get_sessionid(_, undefined, _, _, _) -> {error, developerid};
get_sessionid(_, _, undefined, _, _) -> {error, useremail};
get_sessionid(_, _, _, undefined, _) -> {error, password};
get_sessionid(_, _, _, _, undefined) -> {error, geohash};
get_sessionid(MRServer, DeveloperID, UserEmail, Password, GeoHash) ->
	MRServerBin = erlang:binary_to_list(MRServer),
	case gun:open(MRServerBin, 443) of
		{ok, ConnPid} ->
			Response = case gun:await_up(ConnPid, 5000) of
				{ok, _Protocol} ->
					StreamRef = gun:get(ConnPid, "/api/sessions", [
						{<<"accept">>, <<"application/json">>},
						{<<"useremail">>, UserEmail},
						{<<"password">>, Password},
						{<<"developerid">>, DeveloperID},
						{<<"location">>, GeoHash},
						{<<"token">>, <<"MRServerConnect">>}
					]),
					case gun:await(ConnPid, StreamRef, 5000) of
						{response, fin, _Status, _Headers} ->
							{error, no_data};
						{response, nofin, Status, _Headers} ->
							case Status of
								200 -> extract_sessionid(gun:await_body(ConnPid, StreamRef));
								401 -> {error, unauthorized};
								404 -> {error, unauthorized};
								_ -> {error, connect}
							end
					end;
				{error, _} ->
					{error, get_session}
			end,
			gun:close(ConnPid),
			Response;
		{error, _} ->
			{error, connect}
	end.

extract_sessionid({ok, Message}) ->
	{ok, ej:get({<<"sessionid">>}, imersia_misc:safely_decode_json(Message))};
extract_sessionid({error, Reason}) -> Reason.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the UserID from the SessionID
% ----------------------------------------------------------------------------------------------------
get_userid(MRServer, DeveloperID, UserEmail, SessionID, GeoHash) ->
	MRServerBin = erlang:binary_to_list(MRServer),
	case gun:open(MRServerBin, 443) of
		{ok, ConnPid} ->
			Response = case gun:await_up(ConnPid, 5000) of
				{ok, _Protocol} ->
					StreamRef = gun:get(ConnPid, "/api/user", [
						{<<"accept">>, <<"application/json">>},
						{<<"useremail">>, UserEmail},
						{<<"sessionid">>, SessionID},
						{<<"developerid">>, DeveloperID},
						{<<"location">>, GeoHash}
					]),
					case gun:await(ConnPid, StreamRef, 5000) of
						{response, fin, _Status, _Headers} ->
							{error, no_data};
						{response, nofin, Status, _Headers} ->
							case Status of
								200 -> extract_userid(gun:await_body(ConnPid, StreamRef));
								401 -> {error, unauthorized};
								404 -> {error, unauthorized};
								_ -> {error, connect}
							end
					end;
				{error, _} ->
					{error, get_userid}
			end,
			gun:close(ConnPid),
			Response;
		{error, _} ->
			{error, connect}
	end.

extract_userid({ok, Message}) ->
	{ok, ej:get({<<"userid">>}, imersia_misc:safely_decode_json(Message))};
extract_userid({error, Reason}) -> Reason.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the UserID from the SessionID
% ----------------------------------------------------------------------------------------------------
connect_websocket(MRServer, DeveloperID, SessionID, UserID, GeoHash) ->
	MRServerBin = erlang:binary_to_list(MRServer),
	case gun:open(MRServerBin, 443, #{protocols => [http]}) of
		{ok, ConnPid} ->
			Response = case gun:await_up(ConnPid, 5000) of
				{ok, _Protocol} ->
					StreamRef = gun:ws_upgrade(ConnPid, "/wotcha/" ++ binary_to_list(UserID), [
						{<<"accept">>, <<"application/json">>},
						{<<"sessionid">>, SessionID},
						{<<"developerid">>, DeveloperID},
						{<<"location">>, GeoHash}
					]),
					{ok, StreamRef};
				{error, _} ->
					{error, websocket}
			end,
			% gun:close(ConnPid),
			Response;
		{error, _} ->
			{error, connect}
	end.
% ----------------------------------------------------------------------------------------------------
