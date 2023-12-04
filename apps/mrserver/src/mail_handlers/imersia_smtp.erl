% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: SMTP mail server sender using gen_smtp
% ----------------------------------------------------------------------------------------------------

-module(imersia_smtp).

-export([send_email/3, send_emails/3]).

-type single_response() :: {ok, sent} | {error, atom()}.

-spec send_email (RecipientEmail :: binary(), Subject :: binary(), Message :: binary()) -> single_response().
-spec send_emails (RecipientEmails :: list(binary()), Subject :: binary(), Message :: binary()) -> list(single_response()).



% ----------------------------------------------------------------------------------------------------
% Send same email to a list of recipients
% ----------------------------------------------------------------------------------------------------
send_emails([], _, _) -> [];
send_emails(Recipients, Subject, Message) ->
	AllResults = do_send_emails(Recipients, Subject, Message),
	process_results(AllResults).

do_send_emails([], _, _) -> [];
do_send_emails([Recipient | Recipients], Subject, Message) ->
	[send_email(Recipient, Subject, Message) | do_send_emails(Recipients, Subject, Message)].

process_results([]) -> {ok, sent};
process_results([{ok, sent} | Results]) -> process_results(Results);
process_results([{error, Reason} | _]) -> {error, Reason}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send email to a single recipient
% ----------------------------------------------------------------------------------------------------
send_email( RecipientEmail, Subject, Message ) ->
	RecipientEmailString = check_is_email(erlang:binary_to_list(RecipientEmail)),
	do_send_email( get_smtp_parameters(), {RecipientEmailString, Subject, Message}).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the parameters and send the email
% ----------------------------------------------------------------------------------------------------
do_send_email( {undefined, _, _, _, _}, _) -> {error, relay};
do_send_email( {_, undefined, _, _, _}, _) -> {error, username};
do_send_email( {_, _, undefined, _, _}, _) -> {error, password};
do_send_email( {_, _, _, undefined, _}, _) -> {error, from};
do_send_email( {_, _, _, _, undefined}, _) -> {error, ssl};
do_send_email( _, {undefined, _, _, _}) -> {error, email};
do_send_email( _, {_, undefined, _}) -> {error, subject};
do_send_email( _, {_, _, undefined}) -> {error, message};
do_send_email( {Relay, Username, Password, From, SSL}, {Recipient, Subject, Message}) ->
	Email = mimemail:encode({<<"text">>, <<"html">>, [{<<"Subject">>, Subject}, {<<"From">>, From}], #{}, Message}, []),
	case gen_smtp_client:send({Username, [Recipient], Email}, [{relay, Relay}, {username, Username}, {password, Password}, {ssl, SSL}, {retries, 10}, {auth, always}]) of
		{ok, _} -> {ok, sent};
		_ -> {error, not_sent}
	end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get and test the SMTP parameters from the ini file.
% ----------------------------------------------------------------------------------------------------
get_smtp_parameters() ->
	{
		get_and_convert(smtp, relay),
		get_and_convert(smtp, username),
		get_and_convert(smtp, password),
		get_and_convert_to_binary(smtp, from),
		case imersia_settings:get_setting(smtp, ssl) of <<"false">> -> false; _ -> true end
	}.

get_and_convert(Category, Key) ->
	check_if_empty(erlang:binary_to_list(imersia_settings:get_setting(Category, Key))).

get_and_convert_to_binary(Category, Key) ->
	check_if_empty(imersia_settings:get_setting(Category, Key)).

check_if_empty("") -> undefined;
check_if_empty(<<"">>) -> undefined;
check_if_empty(undefined) -> undefined;
check_if_empty(null) -> undefined;
check_if_empty("undefined") -> undefined;
check_if_empty("null") -> undefined;
check_if_empty(Value) -> Value.

check_is_email(Email) ->
	case string:str(Email, "@") of
		0 -> undefined;
		_ -> Email
	end.
% ----------------------------------------------------------------------------------------------------
