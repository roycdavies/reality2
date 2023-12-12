% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of user functions using Mnesia
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_mnesia_user).

-include("../imersia_datatypes.hrl").

-export([
    user_id/2, user_email/2, user_id_from_sessionid/2, user_new/5, user_list/1, user_login/4, user_logout/2, user_delete/3, user_session_new/3, user_session_new/4,
    user_session_exists/3, user_get_details/2, user_get_details_by_userid/2, user_set_details/3, user_change_password/3
]).


% ----------------------------------------------------------------------------------------------------
% Get the User ID from the user name.
% Returns either the UserID as a string or an appropriate error message.
% ----------------------------------------------------------------------------------------------------
user_id(_Connection, UserEmail) ->
    % Grab all the users with this User name (there should only be one)
    Fun = fun () -> mnesia:index_read(imersia_users_users, UserEmail, #user.useremail) end,
    UserList = mnesia:transaction(Fun),

    % Return whether this user exists or not, and if they do exist, return the User ID
    case UserList of
        {atomic, []} -> {error, useremail};
        {atomic, [User | _]} ->
            {ok, User#user.userid};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the UserEmail from the user ID.
% Returns either the UserEmail as a string or an appropriate error message.
% ----------------------------------------------------------------------------------------------------
user_email(_Connection, UserID) ->
    % Grab all the users with this User name (there should only be one)
    Fun = fun () -> mnesia:read(imersia_users_users, UserID) end,
    UserList = mnesia:transaction(Fun),

    % Return whether this user exists or not, and if they do exist, return the User ID
    case UserList of
        {atomic, []} -> {error, useremail};
        {atomic, [User | _]} ->
            {ok, User#user.useremail};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the User ID from a Session ID, or an appropriate error message
% ----------------------------------------------------------------------------------------------------
user_id_from_sessionid(_Connection, SessionID) ->
    Fun = fun () -> mnesia:read(imersia_users_sessions, SessionID) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, []} -> {error, sessionid};
        {atomic, [Session]} ->
            {ok, Session#session.userid};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete or renew the session, depending on whether it has expired or not.
% ----------------------------------------------------------------------------------------------------
delete_or_renew_session(_Connection, Session) ->
    Created = Session#session.created,
    SessionID = Session#session.sessionid,
    UserID = Session#session.userid,
    Token = Session#session.token,
    % Work out the time difference in seconds
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds(iso8601:parse(Created)),
    if
        (TimeDiff >= 7200) ->
            % Delete it if expired after two hours
            Fun = fun () -> mnesia:delete({imersia_users_sessions, SessionID}) end,
            _Result = mnesia:transaction(Fun),
            {ok, deleted};
        true ->
            % Renew if not expired
            DBEntry = #session {
                sessionid = SessionID,
                token = Token,
                userid = UserID,
                created = iso8601:format(calendar:universal_time())
            },
            Fun = fun () -> mnesia:write(imersia_users_sessions, DBEntry, write) end,
            Result = mnesia:transaction(Fun),
            % Check the result was not an error
            case Result of
                {atomic, ok} ->
                    {ok, updated};
                % Some miscellaneous error
                _ -> {error, database}
            end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new user in the imersia_users database
% Returns either ok or error depending on success or not.
% If a UserID is given, creates a user with that specified ID - this is for copying the old database
% TODO: Check user with that ID doesn't already exist
% ----------------------------------------------------------------------------------------------------
user_new(Connection, UserEmail, Password, UserDetails, Location) ->
    % Check this user doesn't already exist
    UserID = user_id(Connection, UserEmail),
    case UserID of
        % User doesn't yet exist
        {error, useremail} ->
            % Encrypt the password
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, EncryptedPasswordString} = bcrypt:hashpw(binary_to_list(Password), Salt),
            EncryptedPassword = imersia_misc:make_binary(EncryptedPasswordString),
            % Create a new UserID
            NewUserID = imersia_db:new_id(),
            % Create the entry for the database
            DBEntry = #user {
                userid = NewUserID,
                useremail = UserEmail,
                password = EncryptedPassword,
                details =  UserDetails,
                location = imersia_misc:unify_location(Location),
                created = iso8601:format(calendar:universal_time()),
                modified = iso8601:format(calendar:universal_time())
            },
            % Send it to the Database
            Fun = fun () -> mnesia:write(imersia_users_users, DBEntry, write) end,
            Result = mnesia:transaction(Fun),
            % Check that the result was not an error
            case Result of
                {atomic, ok} ->
                    imersia_db:metadata_init(Connection, NewUserID),
                    {ok, NewUserID};
                % Some miscellaneous error
                _Error -> {error, database}
            end;
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            % This user already exists
            {error, useremail}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return a list of the users on this node.
% ----------------------------------------------------------------------------------------------------
user_list(_Connection) ->
    AllRecords = fun (Record, Acc) -> [Record | Acc] end,
    Fun = fun() -> mnesia:foldl(AllRecords, [], imersia_users_users) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, UserList} -> {ok, extract_ids(UserList)};
        _ -> {error, database}
    end.

extract_ids([]) -> [];
extract_ids([User | Users]) -> [User#user.userid | extract_ids(Users)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Login with the given user details
% ----------------------------------------------------------------------------------------------------
user_login(_Connection, UserID, UserEmail, Password) ->
    % Get the details of the user from the UserID
    Fun = fun () -> mnesia:read(imersia_users_users, UserID) end,
    Result = mnesia:transaction(Fun),
    % Check whether user exists or not in the DB
    case Result of
        % No user with that ID exists
        {atomic, []} -> {error, userid};
        % Check that the password and useremail are correct
        {atomic, [UserDetails]} ->
            % Extract the useremail and password from the details from the database
            UserEmailinDB = UserDetails#user.useremail,
            PasswordinDB = UserDetails#user.password,
            if
                % Usernames match
                UserEmailinDB == UserEmail ->
                    case verify_password(Password, PasswordinDB) of
                        % Passwords match, so return a Session ID
                        true -> {ok, logged_in};
                        % Passwords don't match
                        false -> {error, password}
                    end;
                % Usernames don't match
                true -> {error, useremail}
            end;
        % User doesn't exist that matches the UserID
        _ -> {error, database}
    end.

verify_password(Password, PasswordinDB) ->
    Hash = binary_to_list(PasswordinDB),
    case bcrypt:hashpw(binary_to_list(Password), Hash) of
        {ok, Hash} -> true;
        _ -> false
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the user's details
% ----------------------------------------------------------------------------------------------------
user_get_details(Connection, SessionID) ->
    SessionDetails = user_id_from_sessionid(Connection, SessionID),
    user_test_and_get_by_userid(Connection, SessionDetails).

user_test_and_get_by_userid(Connection, {ok, UserID}) -> user_get_details_by_userid(Connection, UserID);
user_test_and_get_by_userid(_, {error, Reason}) -> {error, Reason}.

user_get_details_by_userid(_Connection, UserID) ->
    Fun = fun () -> mnesia:read(imersia_users_users, UserID) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> {error, userid};
        {atomic, [UserDetails]} ->
            UserDetailsOut = #user{
                userid = UserDetails#user.userid,
                useremail = UserDetails#user.useremail,
                password = null,
                details = UserDetails#user.details,
                location = UserDetails#user.location,
                created = iso8601:parse(UserDetails#user.created),
                modified = iso8601:parse(UserDetails#user.modified)
            },
            {ok, UserDetailsOut};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the user details.  Any parameters that are not to be changed should be null (or non-existant).
% ----------------------------------------------------------------------------------------------------
user_set_details(Connection, SessionID, NewUserDetails) ->
    SessionDetails = user_id_from_sessionid(Connection, SessionID),
    user_set_details_by_userid(Connection, SessionDetails, NewUserDetails).

user_set_details_by_userid(_Connection, {ok, UserID}, NewUserDetails) ->
    NewRecord = #user{
        userid=null,
        useremail=null,
        password=null,
        created=null,
        modified=iso8601:format(calendar:universal_time()),
        details=NewUserDetails
    },
    Fun = fun () -> mnesia:read(imersia_users_users, UserID) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, [OldRecord]} ->
            DBEntry = imersia_misc:meld_records(OldRecord, NewRecord),
            Fun2 = fun () -> mnesia:write(imersia_users_users, DBEntry, write) end,
            Result2 = mnesia:transaction(Fun2),
            case Result2 of
                {atomic, ok} ->
                    {ok, updated};
                _ -> {error, database}
            end;
        _ -> {error, sessionid}
    end;
user_set_details_by_userid(_, {error, Reason}, _) ->
    {error, Reason}.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Log out the user with the given session id.
% ----------------------------------------------------------------------------------------------------
user_logout(_Connection, SessionID) ->
    GetFun = fun () -> mnesia:read(imersia_users_sessions, SessionID) end,
    GetResult = mnesia:transaction(GetFun),
    case GetResult of
        {atomic, []} -> {error, sessionid};
        _ ->
            Fun = fun () -> mnesia:delete({imersia_users_sessions, SessionID}) end,
            Result = mnesia:transaction(Fun),
            case Result of
                {atomic, ok} ->
                    {ok, logged_out};
                _ -> {error, database}
            end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Remove all the sessions for a given user that are older than the given number of hours
% ----------------------------------------------------------------------------------------------------
user_remove_all_sessions(Connection, UserID) -> user_remove_all_sessions(Connection, UserID, 0).
user_remove_all_sessions(Connection, UserID, NumHours) ->
    % Get all the sessions for this user
    Fun = fun () -> mnesia:index_read(imersia_users_sessions, UserID, #session.userid) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, Feedback} ->
            % Delete them all
            delete_all_sessions(Connection, Feedback, NumHours),
            {ok, done};
        _ -> {error, database}
    end.
delete_all_sessions(_, [], _) -> ok;
delete_all_sessions(Connection, [Head|Tail], NumHours) ->
    SessionID = Head#session.sessionid,
    Created = iso8601:parse(Head#session.created),
    % Work out the time difference in seconds
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds(Created),
    if
        (TimeDiff >= (NumHours * 3600)) ->
            % Delete it if expired
            Fun = fun () -> mnesia:delete({imersia_users_sessions, SessionID}) end,
            _Result = mnesia:transaction(Fun);
        true -> ok
    end,
    delete_all_sessions(Connection, Tail, NumHours).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the given user, but only if we can confirm with useremail and password
% ----------------------------------------------------------------------------------------------------
user_delete(Connection, SessionID, UserID) ->
    case user_id_from_sessionid(Connection, SessionID) of
        % Check this matches the UserID parameter
        {ok, UserID} ->
            case imersia_db:metadata_drop(Connection, UserID) of
                {ok, deleted} ->
                    Fun = fun () -> mnesia:delete({imersia_users_users, UserID}) end,
                    Result = mnesia:transaction(Fun),
                    case Result of
                        {atomic, ok} ->
                            case imersia_db:channel_delete_all(Connection, UserID) of
                                {ok, deleted} ->
                                    case user_remove_all_sessions(Connection, UserID) of
                                        {ok, done} -> {ok, deleted};
                                        {error, Reason} -> {error, Reason}
                                    end;
                                {error, Reason} -> {error, Reason}
                            end;
                        _ -> {error, database}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {ok, _} -> {error, userid};
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check that this session exists and is valid for this user
% ----------------------------------------------------------------------------------------------------
user_session_exists(Connection, SessionID, UserID) ->
    GetFun = fun () -> mnesia:read(imersia_users_sessions, SessionID) end,
    GetResult = mnesia:transaction(GetFun),
    case GetResult of
        {atomic, []} -> {error, sessionid};
        {atomic, [SessionDetails]} ->
            SessionUserID = SessionDetails#session.userid,
            if
                UserID =:= SessionUserID ->
                case delete_or_renew_session(Connection, SessionDetails) of
                    {ok, updated} -> {ok, exists};
                    _ -> {error, sessionid}
                end;
                true -> {error, userid}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a new session from the User's Email Address and a token (which might come from another Auth Provider)
% first getting the UserID
% The age parameter is only used to set up unit tests to test the removal of expired sessions
% ----------------------------------------------------------------------------------------------------
user_session_new(Connection, Token, UserEmail) -> user_session_new(Connection, Token, UserEmail, 0).
user_session_new(Connection, Token, UserEmail, Age) ->
    % Get the UserID and then create a new session depending on that result
    user_session_new_create(Connection, Token, user_id(Connection, UserEmail), Age).

user_session_new_create(Connection, Token, {ok, UserID}, Age) ->
    % If the user exists, and we get the UserID...
    user_remove_all_sessions(Connection, UserID, ?MAX_SESSION_TIME),
    CreatedTime = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - (Age * 3600)),
    SessionID = imersia_db:new_id(),
    DBEntry = #session {
        sessionid = SessionID,
        token = Token,
        userid = UserID,
        created = iso8601:format(CreatedTime)
    },
    Fun = fun () -> mnesia:write(imersia_users_sessions, DBEntry, write) end,
    Result = mnesia:transaction(Fun),
    % Check on the result was not an error
    case Result of
        {atomic, ok} ->
            {ok, SessionID};
        % Some miscellaneous error
        _ -> {error, database}
    end;
user_session_new_create(_, _, {error, Reason}, _) ->
    {error, Reason}.

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Change the password for a given user
% ----------------------------------------------------------------------------------------------------
user_change_password(Connection, SessionID, NewPassword) ->
    SessionDetails = user_id_from_sessionid(Connection, SessionID),
    user_change_password_by_userid(Connection, SessionDetails, NewPassword).

user_change_password_by_userid(Connection, {ok, UserID}, NewPassword) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, EncryptedPasswordString} = bcrypt:hashpw(binary_to_list(NewPassword), Salt),
    EncryptedPassword = imersia_misc:make_binary(EncryptedPasswordString),

    NewRecord = #user{
        userid=null,
        useremail=null,
        password=EncryptedPassword,
        created=null,
        modified=iso8601:format(calendar:universal_time()),
        details=null
    },
    Fun = fun () -> mnesia:read(imersia_users_users, UserID) end,
    {atomic, [OldRecord]} = mnesia:transaction(Fun),
    DBEntry = imersia_misc:meld_records(OldRecord, NewRecord),
    Fun2 = fun () -> mnesia:write(imersia_users_users, DBEntry, write) end,
    Result = mnesia:transaction(Fun2),
    case Result of
        {atomic, ok} ->
            {ok, done} = user_remove_all_sessions(Connection, UserID),
            {ok, updated};
        _ -> {error, database}
    end;
user_change_password_by_userid(_, {error, Reason}, _) ->
    {error, Reason}.
% ----------------------------------------------------------------------------------------------------
