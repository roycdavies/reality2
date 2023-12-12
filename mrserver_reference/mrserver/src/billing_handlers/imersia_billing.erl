% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Manage billing for each user.  TODO: Create an abstraction layer
% ----------------------------------------------------------------------------------------------------

-module(imersia_billing).

-export([get_num_tokens/1, use_tokens/4, get_transactions/1]).

-spec get_num_tokens (UserEmail :: binary()) -> {ok, created} | {error, atom()}.
-spec use_tokens (UserEmail :: binary(), UserID :: binary(), NumTokens :: number(), Description :: binary()) -> {ok, spent} | {error, atom()}.
-spec get_transactions (UserEmail :: binary()) -> {ok, list()} | {error, atom()}.

% ----------------------------------------------------------------------------------------------------
% Get the number of tokens for a given user (or an error).  If the user does not exist in the billingfox
% engine, create them quietly.
% ----------------------------------------------------------------------------------------------------
get_num_tokens(UserEmail) ->
	case billingfox_api_identify(UserEmail) of
		{ok, Body} ->
			billingfox_num_tokens(Body);
		{error, no_exist} ->
			case billingfox_api_createuser(UserEmail) of
				{ok, created} -> retry_get_num_tokens(UserEmail);
				{error, Reason} -> {error, Reason}
			end;
		{error, Reason} -> {error, Reason}
	end.
retry_get_num_tokens(UserEmail) ->
	case billingfox_api_identify(UserEmail) of
		{ok, Body} ->
			billingfox_num_tokens(Body);
		{error, Reason} -> {error, Reason}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Use tokens from a given user, first making sure they exist.
% ----------------------------------------------------------------------------------------------------
use_tokens(UserEmail, UserID, NumTokens, Description) ->
	case billingfox_api_spend(UserEmail, NumTokens, Description) of
		{ok, Body} ->
			case billingfox_num_tokens_on_spend(Body) of
				{ok, Tokens} ->
					imersia_companion_sup:notify_companion(UserID, {tokens, Tokens}),
					{ok, spent};
				{error, Reason} -> {error, Reason}
			end;
		{error, no_exist} ->
			case billingfox_api_createuser(UserEmail) of
				{ok, created} -> retry_use_tokens(UserEmail, UserID, NumTokens, Description);
				{error, Reason} -> {error, Reason}
			end;
		{error, Reason} -> {error, Reason}
	end.
retry_use_tokens(UserEmail, UserID, NumTokens, Description) ->
	case billingfox_api_spend(UserEmail, NumTokens, Description) of
		{ok, Body} ->
			case billingfox_num_tokens_on_spend(Body) of
				{ok, Tokens} ->
					imersia_companion_sup:notify_companion(UserID, {tokens, Tokens}),
					{ok, spent};
				{error, Reason} -> {error, Reason}
			end;
		{error, Reason} -> {error, Reason}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
get_transactions(_UserEmail) -> [].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the given user's details from BillingFox
% ----------------------------------------------------------------------------------------------------
billingfox_api_identify (UserEmail) ->
	case imersia_settings:get_setting(billing, billingfox) of
		undefined -> {error, settings};
		APIKeyBinary ->
			case gun:open("live.billingfox.com", 443) of
				{ok, ConnPid} ->
					Response = case gun:await_up(ConnPid, 5000) of
						{ok, _Protocol} ->
							%UserEmailEncoded = http_uri:encode(erlang:binary_to_list(UserEmail)),
							%StreamRef = gun:get(ConnPid, "/api/identify?user=" ++ UserEmailEncoded, [
							UserEmailEncoded = uri_string:compose_query([{"user", erlang:binary_to_list(UserEmail)}]),
							StreamRef = gun:get(ConnPid, "/api/identify?user=" ++ UserEmailEncoded, [
								{<<"accept">>, <<"application/json">>},
								{<<"authorization">>, <<"Bearer ", APIKeyBinary/binary>>}
							]),
							case gun:await(ConnPid, StreamRef, 5000) of
								{response, fin, _Status, _Headers} ->
									{error, no_data};
								{response, nofin, Status, _Headers} ->
									case Status of
										200 -> gun:await_body(ConnPid, StreamRef);
										400 -> {error, no_exist};
										_ -> {error, offline}
									end
							end;
						{error, _} ->
							{error, get}
					end,
					gun:close(ConnPid),
					Response;
				{error, _} ->
					{error, connect}
			end
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a user in BillingFox
% ----------------------------------------------------------------------------------------------------
billingfox_api_createuser (UserEmail) ->
	case imersia_settings:get_setting(billing, billingfox) of
		undefined -> {error, settings};
		APIKeyBinary ->
			case gun:open("live.billingfox.com", 443) of
				{ok, ConnPid} ->
					Response = case gun:await_up(ConnPid, 4000) of
						{ok, _Protocol} ->
							% UserEmailEncoded = http_uri:encode(UserEmail),
							% Body = <<"user=", UserEmailEncoded/binary, "&email=", UserEmailEncoded/binary>>,
							Body = uri_string:compose_query([{"user", erlang:binary_to_list(UserEmail)}, {"email", erlang:binary_to_list(UserEmail)}]),
							StreamRef = gun:post(ConnPid, "/api/identify", [
								{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
								{<<"accept">>, <<"application/json">>},
								{<<"authorization">>, <<"Bearer ", APIKeyBinary/binary>>}
							], Body),
							case gun:await(ConnPid, StreamRef, 4000) of
								{response, fin, _Status, _Headers} ->
									{error, no_data};
								{response, nofin, Status, _Headers} ->
									case Status of
										200 -> {ok, created};
										400 -> {error, billing}
									end
							end;
						{error, _} ->
							{error, post}
					end,
					gun:close(ConnPid),
					Response;
				{error, _} ->
					{error, connect}
			end
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
billingfox_api_spend(UserEmail, NumTokens, Description) ->
	NumTokensBinary = number_to_binary(NumTokens),
	case imersia_settings:get_setting(billing, billingfox) of
		undefined -> {error, settings};
		APIKeyBinary ->
			case gun:open("live.billingfox.com", 443) of
				{ok, ConnPid} ->
					Response = case gun:await_up(ConnPid, 4000) of
						{ok, _Protocol} ->
							% UserEmailEncoded = http_uri:encode(UserEmail),
							% DescriptionEncoded = http_uri:encode(Description),
							% Body = <<"user=", UserEmailEncoded/binary, "&amount=", NumTokensBinary/binary, "&description=", DescriptionEncoded/binary>>,
							Body = uri_string:compose_query([{"user", erlang:binary_to_list(UserEmail)}, {"amount", erlang:binary_to_list(NumTokensBinary)}, {"description", erlang:binary_to_list(Description)}]),
							StreamRef = gun:post(ConnPid, "/api/spend", [
								{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
								{<<"accept">>, <<"application/json">>},
								{<<"authorization">>, <<"Bearer ", APIKeyBinary/binary>>}
							], Body),
							case gun:await(ConnPid, StreamRef, 4000) of
								{response, fin, _Status, _Headers} ->
									{error, no_data};
								{response, nofin, Status, _Headers} ->
									case Status of
										200 -> gun:await_body(ConnPid, StreamRef);
										_ -> {error, no_exist}
									end
							end;
						{error, _} ->
							{error, post}
					end,
					gun:close(ConnPid),
					Response;
				{error, _} ->
					{error, connect}
			end
	end.

number_to_binary(Number) when is_float(Number) ->
	erlang:float_to_binary(Number);
number_to_binary(Number) when is_integer(Number) ->
	erlang:integer_to_binary(Number);
number_to_binary(Number) -> Number.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given the data returned from a GET Identify to billingfox, return the number of tokens
% ----------------------------------------------------------------------------------------------------
billingfox_num_tokens(BodyRaw) ->
	Body = imersia_misc:safely_decode_json(BodyRaw),
	case ej:get({<<"user">>, <<"balances">>, <<"available">>}, Body) of
		undefined -> {error, billing};
		TokensString ->
			case imersia_misc:convert_number(TokensString, -1) of
				-1 -> {error, billing};
				Tokens -> {ok, Tokens}
			end
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given the data returned from a GET Identify to billingfox, return the number of tokens
% ----------------------------------------------------------------------------------------------------
billingfox_num_tokens_on_spend(BodyRaw) ->
	Body = imersia_misc:safely_decode_json(BodyRaw),
	case ej:get({<<"balances">>, <<"available">>}, Body) of
		undefined -> {error, billing};
		TokensString ->
			case imersia_misc:convert_number(TokensString, -1) of
				-1 -> {error, billing};
				Tokens -> {ok, Tokens}
			end
	end.
% ----------------------------------------------------------------------------------------------------
