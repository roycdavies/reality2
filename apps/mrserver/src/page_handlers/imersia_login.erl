% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Manage Authentication
% TODO: Add email double-check after registration before allowing user to login
% TODO: Add third-party oAUTH2 confirmation and registration
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_login).
-export([init/2, check/2]).

% ----------------------------------------------------------------------------------------------------
% Interpret the Login commands
% ----------------------------------------------------------------------------------------------------
init(Req, State) ->

	Provider = cowboy_req:binding(provider, Req, <<"">>),
	Command = cowboy_req:binding(command, Req, <<"">>),
	Location = cowboy_req:header(<<"location">>, Req),

	interpret_action(Provider, Command, Location, Req, State).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check if the given user should be given access to the requested resource.
% ----------------------------------------------------------------------------------------------------
check(_Req, _State) ->
	ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Log into the local MR server
% ----------------------------------------------------------------------------------------------------
interpret_action(<<"local">>, <<"login">>, _, Req, State) ->
	% Grab the user name (email) and password
	{ok, Body, Req0} = cowboy_req:read_urlencoded_body(Req),
	UserEmail = ej:get(["email"], {Body}),
	Password = ej:get(["password"], {Body}),

	% Check these against the database
	Connection = imersia_db:connect(),

	case imersia_db:user_id(Connection, UserEmail) of
		% User exists, now have UserID
		{ok, UserID} ->
			case imersia_db:user_login(Connection, UserID, UserEmail, Password) of
				{ok, logged_in} ->
					% All logged in and good - show success page
					% {ok, SessionID} = imersia_db:user_session_new(Connection, imersia_db:new_id(), UserEmail),
					% Req1 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0, #{max_age => 3600}),
					imersia_db:close(Connection),
					imersia_misc:redirect_page(Req0, State, <<"/login/success.html">>);
				{error, _} ->
					% Log in failed
					imersia_db:close(Connection),
					imersia_misc:redirect_page(Req0, State, <<"/login/failure.html?message=Incorrect login details.">>)
			end;
		{error, _} ->
			imersia_db:close(Connection),
			imersia_misc:redirect_page(Req0, State, <<"/login/failure.html?message=User does not exist.">>)
	end;
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Register a new user
% ----------------------------------------------------------------------------------------------------
interpret_action(<<"local">>, <<"register">>, Location, Req, State) ->
	{ok, Body, Req0} = cowboy_req:read_urlencoded_body(Req),
	Firstname = binary_to_list(ej:get(["firstname"], {Body})),
	Surname = binary_to_list(ej:get(["surname"], {Body})),
	UserEmail = binary_to_list(ej:get(["email"], {Body})),
	Nickname = binary_to_list(ej:get(["nickname"], {Body})),
	Password = binary_to_list(ej:get(["password"], {Body})),
	ConfirmPassword = binary_to_list(ej:get(["confirm_password"], {Body})),

	if
		(Password == ConfirmPassword) and (Password /= "") ->
			PasswordOK = test_password(
				(not only_alpha(Password)),
				(not only_numeric(Password)),
				(string:len(Password) > 6),
				(string:to_lower(Password) /= Password),
				(string:to_upper(Password) /= Password)),
			create_user_or_not(PasswordOK, Req0, State, UserEmail, Password, {Firstname, Surname, Nickname}, Location);
		true ->
			imersia_misc:redirect_page(Req0, State, <<"/login/message.html?message=Passwords do not match.&url=/login/register.html">>)
	end.
% ----------------------------------------------------------------------------------------------------
test_password(true, true, true, true, true) -> true;
test_password(_, _, _, _, _) -> false.
% ----------------------------------------------------------------------------------------------------
create_user_or_not(true, Req, State, UserEmail, Password, Details, Location) ->
	Connection = imersia_db:connect(),
	imersia_db:user_new(Connection, UserEmail, Password, Details, Location),
	imersia_db:close(Connection),
	imersia_misc:redirect_page(Req, State, <<"/login/message.html?message=Registered - now log in.&url=/login/index.html">>);
create_user_or_not(false, Req, State, _, _, _, _) ->
	imersia_misc:redirect_page(Req, State, <<"/login/message.html?message=Password must contain alphanumeric and non-alphanumeric characters, upper and lowercase characters, and be longer than 6 characters.&url=/login/register.html">>).
% ----------------------------------------------------------------------------------------------------
only_alpha([Char | Rest]) when Char >= $a, Char =< $z ->
    only_alpha(Rest);
only_alpha([Char | Rest]) when Char >= $A, Char =< $Z ->
    only_alpha(Rest);
only_alpha([]) ->
    true;
only_alpha(_) ->
    false.
% ----------------------------------------------------------------------------------------------------
only_numeric([Char | Rest]) when Char >= $0, Char =< $9 ->
    only_numeric(Rest);
only_numeric([]) ->
    true;
only_numeric(_) ->
    false.
% ----------------------------------------------------------------------------------------------------
