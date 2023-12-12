% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Various License functions
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_license_worker).

-export([start_link/0, loop/1, terminate/2]).



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
start_link() ->
	LicenseWorkerPid = spawn_link(imersia_license_worker, loop, [0]),
	register(license_worker, LicenseWorkerPid),
	license_worker ! do_check,
	{ok, LicenseWorkerPid}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
terminate(_Event, _State) ->
    imersia_misc:debug(debug, "Terminating License Checker~n", []),
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The main loop - receives messages and processes them accordingly
% ----------------------------------------------------------------------------------------------------
loop(LicenseCheckCounter) ->
    receive
		do_check ->
			NewLicenseCheckCounter = case check_license() of
		        {ok, licensed} ->
		            imersia_misc:debug(debug, "Valid License found.~n", []),
					% Check again in an hour
					timer:send_after(3600000, license_worker, do_check),
					0;
		        {error, Reason} ->
					case LicenseCheckCounter > 20 of
						true ->
							% Remove the developer id set if we can't find a valid license after 20 tries.
							% This also ensures that once a license is obtained, it takes 20 tries to invalidate it
							% thus ensuring if the license server is offline a while, it doesn't immediately affect
							% other MRServers.
							application:set_env(mrserver, developerid, sets:from_list([]), [{persistent, true}]),
							imersia_misc:debug(debug, "Invalid License (time-out)- ~w.~n", [Reason]),
							% Check again in 10 minutes
							timer:send_after(600000, license_worker, do_check);
						false ->
							imersia_misc:debug(debug, "Invalid License (checking again) - ~w.~n", [Reason]),
							% Check again in 1 minute
							timer:send_after(60000, license_worker, do_check)
					end,
					LicenseCheckCounter + 1
		    end,
			loop(NewLicenseCheckCounter);
		_ ->
			loop(LicenseCheckCounter)
	end,
	ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
check_license() ->
    LicenseCode = case application:get_env(mrserver, binary_to_atom(<<"license_code">>, utf8)) of
        {ok, Val} when is_atom(Val) -> atom_to_binary(Val, utf8);
        {ok, Val} when is_list(Val) -> list_to_binary(Val);
        {ok, Val} when is_binary(Val) -> Val;
        _ -> undefined
    end,

    case LicenseCode of
        undefined -> {error, no_license};
        EncryptedLicenseDetails ->
            LicenseJSON = try decrypt_license(EncryptedLicenseDetails) of
                Result -> Result
            catch
                _:_ -> [{}]
            end,
						erlang:display(LicenseJSON),
            Server = ej:get({<<"mrserver">>}, LicenseJSON),
            ID = ej:get({<<"id">>}, LicenseJSON),
            Keys = ej:get({"keys"}, LicenseJSON),
			ServerURL = imersia_settings:get_setting(mrserver, url),
			% erlang:display(Server),
			% erlang:display(ServerURL),
            try get_developer_id(Server, ServerURL, ID, Keys) of
                Result2 ->
					send_event_to_license_server (ID, Keys, <<"check">>, [{<<"mrserver">>, Server}]),
					Result2
            catch
                _:_ ->
					send_event_to_license_server (ID, Keys, <<"error">>, [{<<"mrserver">>, Server}]),
					{error, license_server_offline}
            end
    end.

get_developer_id(<<"development">>, _, _, _) ->
    DeveloperIDsSet = sets:from_list([<<"test">>, <<"Test">>, <<"com.imersia.geojson">>, <<"com.imersia.portal">>, <<"com.imersia.webapp">>]),
    application:set_env(mrserver, developerid, DeveloperIDsSet, [{persistent, true}]),
    {ok, licensed};
get_developer_id(undefined, _, _, _) -> {error, invalid_license};
get_developer_id(_, undefined, _, _) -> {error, no_server_url_in_ini};
get_developer_id(_, _, undefined, _) -> {error, invalid_license};
get_developer_id(_, _, _, undefined) -> {error, invalid_license};
get_developer_id(Server, Server, ID, Keys) ->
    ServerString = "portal.imersia.net",
    case gun:open(ServerString, 443) of
        {ok, ConnPid} ->
            Response = case gun:await_up(ConnPid, 5000) of
                {ok, _Protocol} ->
                    StreamRef = gun:get(ConnPid, "/api/metadata", [
                        {<<"accept">>, <<"application/json">>},
                        {<<"developerid">>, <<"Test">>},
						{<<"location">>, <<"rzzzzzzzzzzz">>},
                        {<<"key">>, Server},
                        {<<"geobotid">>, ID},
                        {<<"contextids">>, imersia_misc:safely_encode_json(Keys)}
                    ]),
                    case gun:await(ConnPid, StreamRef, 5000) of
                        {response, fin, _Status, _Headers} ->
                            {error, license_server_offline};
                        {response, nofin, Status, _Headers} ->
                            case Status of
                                200 -> extract_license_code(gun:await_body(ConnPid, StreamRef));
                                401 -> {error, invalid_license};
                                404 -> {error, invalid_license};
                                _ -> {error, invalid_license}
                            end
                    end;
                {error, _} ->
                    {error, license_server_offline}
            end,
            gun:close(ConnPid),
            Response;
        {error, _} ->
            {error, license_server_offline}
    end;
get_developer_id(_, _, _, _) -> {error, license_does_not_match_with_ini_url}.


extract_license_code({ok, Message}) ->
    DeveloperIDs = ej:get({<<"value">>}, imersia_misc:safely_decode_json(Message)),
    DeveloperIDsSet = sets:from_list(DeveloperIDs),
    application:set_env(mrserver, developerid, DeveloperIDsSet, [{persistent, true}]),
	{ok, licensed};
extract_license_code({error, _}) -> {error, invalid_license}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
send_event_to_license_server (GeobotID, Keys, Event, Parameters) ->
	ServerString = "portal.imersia.net",
	case gun:open(ServerString, 443) of
		{ok, ConnPid} ->
			Response = case gun:await_up(ConnPid, 5000) of
				{ok, _Protocol} ->
					Body = [{<<"event">>, Event}, {<<"parameters">>, Parameters}],
					BodyJSON = imersia_misc:safely_encode_json(Body),
					StreamRef = gun:post(ConnPid, "/api/geobots/log", [
						{<<"content-type">>, <<"application/json">>},
						{<<"developerid">>, <<"Test">>},
						{<<"location">>, <<"rzzzzzzzzzzz">>},
						{<<"geobotid">>, GeobotID},
						{<<"contextids">>, imersia_misc:safely_encode_json(Keys)}
					], BodyJSON),
					case gun:await(ConnPid, StreamRef, 4000) of
						{response, fin, _Status, _Headers} ->
							{error, no_data};
						{response, nofin, Status, _Headers} ->
							case Status of
								200 ->
									imersia_misc:debug(debug, "License event sent~n", []),
									{ok, sent};
								_ ->
									imersia_misc:debug(debug, "Error sending license event~n", []),
									{error, event}
							end
					end;
				{error, _} ->
					{error, license_server_offline}
			end,
			gun:close(ConnPid),
			Response;
		{error, _} ->
			{error, license_server_offline}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given a raw license from the mrserver.toml file, return the JSON that this embodies.
% ----------------------------------------------------------------------------------------------------
decrypt_license(RawLicense) ->
    % Get the Public Key
    PrivDir = code:priv_dir(mrserver),

    {ok, RawPKey} = file:read_file(PrivDir ++ "/public.pem"),

    % Put it into a form usable by the crypto libraries
    [EncPKey] = public_key:pem_decode(RawPKey),
    PKey = public_key:pem_entry_decode(EncPKey),

    % Decode the license from base64
    DecodedBase64 = base64:decode(RawLicense),

    % Decrypt it using the public key
    LicenseJSON = public_key:decrypt_public(DecodedBase64, PKey),
		% erlang:display(LicenseJSON),

    % Turn the resulting text into JSON structures.
    imersia_misc:safely_decode_json(LicenseJSON).
% ----------------------------------------------------------------------------------------------------
