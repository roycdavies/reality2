% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The User Password Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_user_password_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_accepted/2,
    from_json/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    UserID = case cowboy_req:binding(userid, Req) of
        undefined -> cowboy_req:header(<<"userid">>, Req);
        UserIDParam -> UserIDParam
    end,
    SessionID = cowboy_req:header(<<"sessionid">>, Req),

    Passcode = cowboy_req:header(<<"passcode">>, Req),
    UserEmail = cowboy_req:header(<<"useremail">>, Req),

    Connection = imersia_db:connect(),

    Parameters = #{connection => Connection, userid => UserID, sessionid => SessionID, passcode => Passcode, useremail => UserEmail},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
    Location = cowboy_req:header(<<"location">>, Req2),
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} -> {true, Req2, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {{false, atom_to_list(Reason)} , Req2, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"PUT">>], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    NewPassword = cowboy_req:header(<<"password">>, Req),
    #{parameters := #{connection := Connection, userid := UserID, sessionid := SessionID, passcode := Passcode, useremail := UserEmail}} = Req,

    case NewPassword of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(password, Req, State);
        _ ->
            case Passcode of
                undefined ->
                    case imersia_db:user_exists_and_matches(Connection, SessionID, UserID, undefined) of
                        {ok, matches} ->
                            case imersia_db:user_change_password(Connection, SessionID, NewPassword) of
                                {ok, updated} ->
                                    imersia_db:close(Connection),
                                    {true, cowboy_req:set_resp_body([<<"{\"userid\":\"">>, UserID, <<"\"}">>], Req), State};
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason, Req, State)
                            end;
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(Reason, Req, State)
                    end;
                _ ->
                    case imersia_db:user_session_new(Connection, Passcode, UserEmail) of
                        {ok, NewSessionID} ->
                            case imersia_db:passcode_check(Connection, UserEmail, Passcode) of
                                {ok, checked} ->
                                    case imersia_db:user_change_password(Connection, NewSessionID, NewPassword) of
                                        {ok, updated} ->
                                            case imersia_db:user_id(connection, UserEmail) of
                                                {ok, NewUserID} ->
                                                    imersia_db:close(Connection),
                                                    {true, cowboy_req:set_resp_body([<<"{\"userid\":\"">>, NewUserID, <<"\"}">>], Req), State};
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
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(Reason, Req, State)
                    end
            end
    end.
% ----------------------------------------------------------------------------------------------------
