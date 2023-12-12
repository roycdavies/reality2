% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Useful License functions
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_license).

-export([generate_license/4, check_license/0]).



% ----------------------------------------------------------------------------------------------------
% Check the license
% ----------------------------------------------------------------------------------------------------
check_license() ->
	case whereis(license_worker) of
		undefined ->
			imersia_misc:debug(debug, "License Checker is not functioning.~n", []),
			{error, license_checker_dead};
		_ ->
			imersia_misc:debug(debug, "Checking License.~n", []),
			license_worker ! do_check,
			{ok, checking}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create the license string to put into the mrserver.toml file.  Use these commands to make the public
% and private keys.
% openssl genrsa -out private.pem 2048
% openssl rsa -in private.pem -out public.pem -outform PEM -pubout
% ----------------------------------------------------------------------------------------------------
generate_license(undefined, _, _, _) -> undefined;
generate_license(_, undefined, _, _) -> undefined;
generate_license(_, _, undefined, _) -> undefined;
generate_license(_, _, _, undefined) -> undefined;
generate_license(Server, ID, Type, Keys) ->
    SSLDir = binary_to_list(imersia_settings:get_setting(mrserver, ssldir)),

	case file:read_file(SSLDir ++ "/private.pem") of
		{ok, RawSKey} ->
		    [EncSKey] = public_key:pem_decode(RawSKey),
		    SKey = public_key:pem_entry_decode(EncSKey),
		    LicenseDetails = [{<<"mrserver">>, Server}, {<<"id">>, ID}, {<<"keys">>, Keys}, {<<"type">>, Type}],
		    Msg = imersia_misc:safely_encode_json(LicenseDetails),
		    EncryptedMessage = public_key:encrypt_private(Msg, SKey),
		    LicenseCode = base64:encode(EncryptedMessage),
			% erlang:display(LicenseCode),
			LicenseCode;
		_ -> undefined
	end.
% ----------------------------------------------------------------------------------------------------

% new_license(UserEmail, ServerURL, ApplicationIDs) ->
	% Get ChannelID of '__licenses' channel

	% Get List of Geobots on the channel

	% Check if Geobot exists with name 'ClientName' - if so, use that geobotid
