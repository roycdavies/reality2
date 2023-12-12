% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Session Handler for the API
% GET       - Gets a new Session ID given the user's email address and password in the header
% HEAD      -
% OPTIONS   - Returns the allowed methods (GET, HEAD, OPTIONS, DELETE)
% DELETE    - Removes (makes invalid) a SessionID sent in the header
%
% TODO: Check and Log the DeveloperID and Location details
% TODO: Set / User Cookies
% ----------------------------------------------------------------------------------------------------
-module(imersia_sessions_api).

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_json/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Get a possible token from the header (from other AUTH system), or generate one
    Token = case cowboy_req:header(<<"token">>, Req) of
        undefined -> imersia_db:new_id();
        TokenIn -> TokenIn
    end,

    % Get the Authorisation parameters
    Parameters = case cowboy_req:parse_header(<<"authorization">>, Req, false) of
        {basic, UserEmail, Password} ->
            #{useremail => UserEmail, password => Password, passcode => undefined, token => Token};
        _ ->
            #{useremail => cowboy_req:header(<<"useremail">>, Req), password => cowboy_req:header(<<"password">>, Req), passcode => cowboy_req:header(<<"passcode">>, Req), token => Token}
    end,
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
    Location = cowboy_req:header(<<"location">>, Req2),
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} -> {true, Req2, State};
        {error, Reason} -> {{false, atom_to_list(Reason)} , Req2, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check if the parameters sent to GET or DELETE a session ID are valid
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) -> do_resource_exists(cowboy_req:method(Req), Req, State).

% A DELETE Session
do_resource_exists(<<"DELETE">>, Req, State) ->
    % Get a connection to the database
    Connection = imersia_db:connect(),

    % Get the sessionid either from the header or the URL
    SessionID = case cowboy_req:binding(sessionid, Req) of
        undefined -> cowboy_req:header(<<"sessionid">>, Req);
        SessionIDParam -> SessionIDParam
    end,

    case cowboy_req:header(<<"useremail">>, Req) of
        % No Email Address passed in = error
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(useremail, Req, State);

        % Email address passed in, check it matches the SessionID
        UserEmail ->
            % Get the UserID from the UserEmail
            case imersia_db:user_id(Connection, UserEmail) of
                {ok, UserID} ->
                    if
                        % No SessionID passed in
                        (SessionID == undefined) ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(sessionid, Req, State);
                        true ->
                            % Check the SessionID belongs to this user
                            case (imersia_db:user_session_exists(Connection, SessionID, UserID)) of
                                % All valid, log the user out.
                                {ok, exists} ->
                                    case imersia_db:user_logout(Connection, SessionID) of
                                        {ok, logged_out} ->
                                            imersia_db:close(Connection),
                                            {true, Req, State};
                                        {error, Reason} ->
                                            imersia_db:close(Connection),
                                            imersia_misc:response_error(Reason, Req, State)
                                    end;
                                % Invalid SessionID wrt UserEmail passed in (could be a different user, or might have been logged out already)
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason, Req, State)
                            end
                    end;
                % UserEmail doesn't belong to an existing user
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end;

% A GET Session
do_resource_exists(<<"GET">>, Req, State) ->
    % Get a connection to the database
    Connection = imersia_db:connect(),

    % Grab the parameters passed in
    % #{parameters := #{useremail := UserEmail, password := Password, passcode := Passcode}} = Req,
    #{parameters := #{useremail := UserEmail, password := Password, passcode := Passcode, token := Token}} = Req,

    LoginProcess = imersia_settings:get_setting(authentication, login),

    check_authentication(Connection, UserEmail, Password, Passcode, Token, Req, State, LoginProcess);

do_resource_exists(<<"HEAD">>, Req, State) ->
    % Get a connection to the database
    Connection = imersia_db:connect(),

    % Get the sessionid either from the header or the URL
    SessionID = case cowboy_req:binding(sessionid, Req) of
        undefined -> cowboy_req:header(<<"sessionid">>, Req);
        SessionIDParam -> SessionIDParam
    end,

    case cowboy_req:header(<<"useremail">>, Req) of
        % No Email Address passed in = error
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(useremail, Req, State);

        % Email address passed in, check it matches the SessionID
        UserEmail ->
            % Get the UserID from the UserEmail
            case imersia_db:user_id(Connection, UserEmail) of
                {ok, UserID} ->
                    if
                        % No SessionID passed in
                        (SessionID == undefined) ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(sessionid, Req, State);
                        true ->
                            % Check the SessionID belongs to this user
                            case (imersia_db:user_session_exists(Connection, SessionID, UserID)) of
                                % All valid, log the user out.
                                {ok, exists} ->
                                    imersia_db:close(Connection),
                                    {true, Req, State};
                                % Invalid SessionID wrt UserEmail passed in (could be a different user, or might have been logged out already)
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason, Req, State)
                            end
                    end;
                % UserEmail doesn't belong to an existing user
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end
    end;

% Any other Session type
do_resource_exists(_, Req, State) -> {true, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the various forms of authentication possibilities
% ----------------------------------------------------------------------------------------------------
check_authentication(Connection, undefined, _, _, _, Req, State, _) ->
    % Need an email address
    imersia_db:close(Connection),
    imersia_misc:response_error(useremail, Req, State);
check_authentication(Connection, _, undefined, _, _, Req, State, <<"twofactor">>) ->
    % Password is undefined and the login process is two factor
    imersia_db:close(Connection),
    imersia_misc:response_error(password, Req, State);
check_authentication(Connection, _, _, undefined, _, Req, State, <<"twofactor">>) ->
    % Passcode is undefined and the login process is two factor
    imersia_db:close(Connection),
    imersia_misc:response_error(passcode, Req, State);
check_authentication(Connection, _, _, undefined, _, Req, State, <<"passcode">>) ->
    % Passcode is undefined and the login process requires it
    imersia_db:close(Connection),
    imersia_misc:response_error(passcode, Req, State);
check_authentication(Connection, _, undefined, _, _, Req, State, LoginProcess) when ((LoginProcess =/= <<"twofactor">>) and (LoginProcess =/= <<"passcode">>)) ->
    % Password is undefined and the login process requires it
    % password based login is the default and fall back if authentication is not specified at all
    imersia_db:close(Connection),
    imersia_misc:response_error(password, Req, State);
check_authentication(Connection, UserEmail, _, Passcode, Token, Req, State, <<"passcode">>) ->
    % Normal useremail / passcode log in
    case imersia_db:user_id(Connection, UserEmail) of
        {ok, _UserID} ->
            case imersia_db:passcode_check(Connection, UserEmail, Passcode) of
                {ok, checked} ->
                    Req2 = Req#{parameters => #{useremail => UserEmail, passcode => Passcode, token => Token}},
                    imersia_db:close(Connection),
                    {true, Req2, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end;
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;
check_authentication(Connection, UserEmail, Password, Passcode, Token, Req, State, <<"twofactor">>) ->
    % Two factor authentication
    case imersia_db:user_id(Connection, UserEmail) of
        {ok, UserID} ->
            case imersia_db:passcode_check(Connection, UserEmail, Passcode) of
                {ok, checked} ->
                    case imersia_db:user_login(Connection, UserID, UserEmail, Password) of
                        {ok, logged_in} ->
                            Req2 = Req#{parameters => #{useremail => UserEmail, passcode => Passcode, password => Password, token => Token}},
                            imersia_db:close(Connection),
                            {true, Req2, State};
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(Reason, Req, State)
                    end;
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end;
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;
check_authentication(Connection, UserEmail, Password, _, Token, Req, State, _) ->
    % Normal useremail / password log in (and default fallback)
    case imersia_db:user_id(Connection, UserEmail) of
        {ok, UserID} ->
            case imersia_db:user_login(Connection, UserID, UserEmail, Password) of
                {ok, logged_in} ->
                    Req2 = Req#{parameters => #{useremail => UserEmail, password => Password, token => Token}},
                    imersia_db:close(Connection),
                    {true, Req2, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Reason, Req, State)
            end;
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Tidy up after a sessionID was deleted
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Already deleted in resource_exists function
    Req0 = cowboy_req:set_resp_body(<<"">>, Req),
    {true, Req0, State}.
delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET or HEAD
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            % Grab the parameters passed in
            #{parameters := #{useremail := UserEmail, token := Token}} = Req,
            % Get a connection to the database
            Connection = imersia_db:connect(),
            % All good - we've got this far and logged in successfully, so issue a new SessionID
            case imersia_db:user_session_new(Connection, Token, UserEmail) of
                {ok, SessionID} ->
                    imersia_db:close(Connection),
                    % Return the SessionID
                    {jsx:encode([{sessionid, SessionID}]), Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {jsx:encode([{error, Reason}]), Req, State}
            end;

        <<"HEAD">> ->
            {"", Req, State}
    end.
% ----------------------------------------------------------------------------------------------------
