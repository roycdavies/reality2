% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The User Handler for the API
% GET       - Get a user's details
% POST      - Create a new user
% PUT       - Update an existing user
% HEAD      -
% OPTIONS   - Returns the allowed methods (GET, HEAD, OPTIONS, DELETE, POST, PUT)
% DELETE    - Removes (makes invalid) a SessionID sent in the header
%
% TODO: Check and Log the DeveloperID and Location details
% TODO: Set / User Cookies
% ----------------------------------------------------------------------------------------------------
-module(imersia_user_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2,
    is_authorized/2,
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
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check Authorization
% TODO: Allow only certain users to create new users
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Get all the parameters
    Connection = imersia_db:connect(),
    UserID = case cowboy_req:binding(userid, Req) of
        undefined -> cowboy_req:header(<<"userid">>, Req);
        UserIDParam -> UserIDParam
    end,
    SessionID = cowboy_req:header(<<"sessionid">>, Req),
    UserEmail = cowboy_req:header(<<"useremail">>, Req),
    Passcode = cowboy_req:header(<<"passcode">>, Req),

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req),
    Location = cowboy_req:header(<<"location">>, Req),

    % Add these to the stream for later
    Parameters = #{useremail => UserEmail, sessionid => SessionID, userid => UserID, connection => Connection, location => Location, passcode => Passcode},
    Req2 = Req#{parameters => Parameters},

    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} -> {true, Req2, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {{false, atom_to_list(Reason)} , Req2, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether a User, as given by the header and binding info, exists or not.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) -> do_resource_exists(cowboy_req:method(Req), Req, State).

do_resource_exists(_, Req, State) ->
    #{parameters := #{connection := Connection, userid := UserID, useremail := UserEmail, sessionid := SessionID}} = Req,

    UserExists = imersia_db:user_exists_and_matches(Connection, SessionID, UserID, UserEmail),
    case UserExists of
        {ok, _} -> {true, Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete user
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, userid := UserID, sessionid := SessionID}} = Req,

    case imersia_db:user_delete(Connection, SessionID, UserID) of
        {ok, deleted} ->
            imersia_db:close(Connection),
            Req0 = cowboy_req:set_resp_body(<<"">>, Req),
            {true, Req0, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.

delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, sessionid := SessionID}} = Req,

    case cowboy_req:method(Req) of
        <<"GET">> ->
            case imersia_db:user_get_details(Connection, SessionID) of
                {ok, UserDetails} ->
                    UserDetailsJSON = imersia_misc:record_to_json(UserDetails, false),
                    imersia_db:close(Connection),
                    {imersia_misc:safely_encode_json(UserDetailsJSON), Req, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {jsx:encode([{error, Reason}]), Req, State}
            end;
        <<"HEAD">> ->
            imersia_db:close(Connection),
            {"", Req, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) -> action_from_json(cowboy_req:method(Req), Req, State).

% A PUT Session (Update an existing user)
action_from_json(<<"PUT">>, Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, userid := UserID, sessionid := SessionID}} = Req,

    % Get the updated user details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(user_details, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(userdetails, Req, State);
        UserDetails ->
            case imersia_db:user_exists_and_matches(Connection, SessionID, UserID, undefined) of
                {ok, matches} ->
                    case imersia_db:user_set_details(Connection, SessionID, UserDetails) of
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
            end
    end;

% A POST Session (Create a new user)
action_from_json(<<"POST">>, Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, location := Location, passcode := Passcode}} = Req,

    % Get the details of the user from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(user, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(user, Req, State);
        User ->
            Details = User#user.details,
            UserEmail = User#user.useremail,
            Password = User#user.password,
            case check_userdetails(Details, UserEmail, Password) of
                ok ->
                    if
                        (UserEmail == undefined) ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(useremail, Req, State);
                        (Password == undefined) ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(password, Req, State);
                        true ->
                            case imersia_db:passcode_check(Connection, UserEmail, Passcode) of
                                _ ->
                                    case imersia_db:user_new(Connection, UserEmail, Password, Details, Location) of
                                        {ok, UserID} ->
                                            imersia_db:close(Connection),
                                            {true, cowboy_req:set_resp_body([<<"{\"userid\":\"">>, UserID, <<"\"}">>], Req), State};
                                        {error, Reason} ->
                                            imersia_db:close(Connection),
                                            imersia_misc:response_error(Reason, Req, State)
                                    end

                                % {ok, checked} ->
                                %     case imersia_db:user_new(Connection, UserEmail, Password, Details, Location) of
                                %         {ok, UserID} ->
                                %             imersia_db:close(Connection),
                                %             {true, cowboy_req:set_resp_body([<<"{\"userid\":\"">>, UserID, <<"\"}">>], Req), State};
                                %         {error, Reason} ->
                                %             imersia_db:close(Connection),
                                %             imersia_misc:response_error(Reason, Req, State)
                                %     end;
                                % {error, Reason} ->
                                %     imersia_db:close(Connection),
                                %     imersia_misc:response_error(Reason, Req, State)
                            end
                    end;
                {incomplete, Parameter} ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(Parameter, Req, State)
            end
    end.

check_userdetails(UserDetails) when is_record(UserDetails, user_details) ->
    check_inside_userdetails (UserDetails#user_details.firstname, UserDetails#user_details.surname, UserDetails#user_details.nickname);
check_userdetails(_) -> {incomplete, details}.

check_userdetails(_, null, _) -> {incomplete, useremail};
check_userdetails(_, _, null) -> {incomplete, password};
check_userdetails(UserDetails, _, _) -> check_userdetails(UserDetails).

check_inside_userdetails(null, _, _) -> {incomplete, firstname};
check_inside_userdetails(_, null, _) -> {incomplete, surname};
check_inside_userdetails(_, _, null) -> {incomplete, nickname};
check_inside_userdetails(_, _, _) -> ok.
% ----------------------------------------------------------------------------------------------------
