% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of user functions using MongoDB
% ----------------------------------------------------------------------------------------------------

-module(imersia_mongo_user).

-include("../imersia_datatypes.hrl").

-export([
    user_id/2, user_email/2, user_id_from_sessionid/2, user_new/5, user_list/1, user_login/4, user_logout/2, user_delete/3, user_session_new/3, user_session_new/4,
    user_session_exists/3, user_get_details/2, user_get_details_by_userid/2, user_set_details/3, user_change_password/3
]).


% ----------------------------------------------------------------------------------------------------
% Get the User ID from the user name.
% Returns either the UserID as a string or an appropriate error message.
% ----------------------------------------------------------------------------------------------------
user_id(_, UserEmail) ->
    % Return whether this user exists or not, and if they do exist, return the User ID
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            % Grab all the users with this User name (there should only be one)
            case mc_worker_api:find_one(Connection, <<"companions">>, #{<<"useremail">> => UserEmail}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, useremail};
                User ->
                    mc_worker_api:disconnect(Connection),
                    {ok, maps:get(<<"_id">>, User)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the User Email from the user ID.
% Returns either the UserEmail as a string or an appropriate error message.
% ----------------------------------------------------------------------------------------------------
user_email(_, UserID) ->
    % Return whether this user exists or not, and if they do exist, return the UserEmail
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            % Grab all the users with this UserID (there should only be one)
            case mc_worker_api:find_one(Connection, <<"companions">>, #{<<"_id">> => UserID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, userid};
                User ->
                    mc_worker_api:disconnect(Connection),
                    {ok, maps:get(<<"useremail">>, User)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the User ID from a Session ID, or an appropriate error message
% TODO: Check if sessionid is too old, and extend its age if not.
% ----------------------------------------------------------------------------------------------------
user_id_from_sessionid(_Connection, SessionID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            % Grab all the sessions with this ID
            case mc_worker_api:find_one(Connection, <<"sessions">>, #{<<"_id">> => SessionID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, sessionid};
                Session ->
                    mc_worker_api:disconnect(Connection),
                    {ok, maps:get(<<"userid">>, Session)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete or renew the session, depending on whether it has expired or not.
% ----------------------------------------------------------------------------------------------------
delete_or_renew_session(Connection, Session) ->
    Created = maps:get(<<"created">>, Session),
    SessionID = maps:get(<<"_id">>, Session),
    % Work out the time difference in seconds
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds(iso8601:parse(Created)),
    if
        (TimeDiff >= 7200) ->
            % Delete it if expired after two hours
            mc_worker_api:delete(Connection, <<"sessions">>, #{<<"_id">> => SessionID}),
            {ok, deleted};
        true ->
            % Renew if not expired
            SessionDetails = #{
                <<"created">> => iso8601:format(calendar:universal_time())
            },
            Command = #{<<"$set">> => SessionDetails},
            mc_worker_api:update(Connection, <<"sessions">>, #{<<"_id">> => SessionID}, Command),
            {ok, updated}
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
            DBEntryMap = maps:remove(<<"userid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

            % Create new database for this Companion / User
            case mc_worker_api:connect ([{database, <<"imersia_", NewUserID/binary>>}]) of
                {ok, Connection2} ->
                    mc_worker_api:insert(Connection2, <<"details">>, DBEntryMap#{<<"_id">> => NewUserID}),
                    mc_worker_api:disconnect(Connection2),

                    % Insert entry into the admin database
                    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
                        {ok, Connection3} ->
                            AdminDBEntry = #{
                                <<"_id">> => NewUserID,
                                <<"useremail">> => UserEmail,
                                <<"location">> => #{
                                    <<"type">> => <<"Point">>,
                                    <<"coordinates">> => [180.0, 0.0, 0.0]
                                }
                            },
                            mc_worker_api:insert(Connection3, <<"companions">>, AdminDBEntry),
                            mc_worker_api:disconnect(Connection3),
                            {ok, NewUserID};
                        _ -> {error, database}
                    end;
                _ -> {error, database}
            end;
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            % This user already exists
            {error, useremail}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return a list of the user IDs on this node.
% ----------------------------------------------------------------------------------------------------
user_list(_Connection) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"companions">>, {}) of
                {ok, Cursor} ->
                    UserList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, UserList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {UserMap}) ->
    [maps:get(<<"_id">>, UserMap) | get_at_cursor(Cursor, mc_cursor:next(Cursor))].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Login with the given user details
% ----------------------------------------------------------------------------------------------------
user_login(_Connection, UserID, UserEmail, Password) ->
    % Return whether this user exists or not, and if they do exist, return the User ID
    case mc_worker_api:connect ([{database, <<"imersia_", UserID/binary>>}]) of
        {ok, Connection} ->
            % Grab all the users with this User name (there should only be one)
            case mc_worker_api:find_one(Connection, <<"details">>, #{<<"_id">> => UserID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, userid};
                UserDetails ->
                    mc_worker_api:disconnect(Connection),
                    % Extract the useremail and password from the details from the database
                    UserEmailinDB = maps:get(<<"useremail">>, UserDetails),
                    PasswordinDB = maps:get(<<"password">>, UserDetails),
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
                    end
            end;
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
    case mc_worker_api:connect ([{database, <<"imersia_", UserID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"details">>, #{<<"_id">> => UserID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, userid};
                UserDetails ->
                    UserNames = maps:get(<<"details">>, UserDetails),
                    UserLocation = maps:get(<<"location">>, UserDetails),
                    UserDetailsOut = #user{
                        userid = maps:get(<<"_id">>, UserDetails),
                        useremail = maps:get(<<"useremail">>, UserDetails),
                        password = null,
                        details = #user_details{
                            firstname = maps:get(<<"firstname">>, UserNames),
                            surname = maps:get(<<"surname">>, UserNames),
                            nickname = maps:get(<<"nickname">>, UserNames),
                            imageurl = maps:get(<<"imageurl">>, UserNames)
                        },
                        location = #location{
                            latitude = maps:get(<<"latitude">>, UserLocation),
                            longitude = maps:get(<<"longitude">>, UserLocation),
                            altitude = maps:get(<<"altitude">>, UserLocation),
                            geohash = maps:get(<<"geohash">>, UserLocation)
                        },
                        created = iso8601:parse(maps:get(<<"created">>, UserDetails)),
                        modified = iso8601:parse(maps:get(<<"modified">>, UserDetails))
                    },
                    mc_worker_api:disconnect(Connection),
                    {ok, UserDetailsOut}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the user details.  Any parameters that are not to be changed should be null (or non-existant).
% ----------------------------------------------------------------------------------------------------
user_set_details(Connection, SessionID, NewUserDetails) ->
    SessionDetails = user_id_from_sessionid(Connection, SessionID),
    user_set_details_by_userid(Connection, SessionDetails, NewUserDetails).

user_set_details_by_userid(_, {ok, UserID}, NewUserDetails) ->
    % Update database entry for this Companion / User
    case mc_worker_api:connect ([{database, <<"imersia_", UserID/binary>>}]) of
        {ok, Connection} ->
            CheckNull = fun(_Key, Value) -> (Value /= null) and (Value /= undefined) end,
            UserDetails = maps:filter(CheckNull, #{
                <<"modified">> => iso8601:format(calendar:universal_time()),
                <<"details.firstname">> => NewUserDetails#user_details.firstname,
                <<"details.surname">> => NewUserDetails#user_details.surname,
                <<"details.nickname">> => NewUserDetails#user_details.nickname,
                <<"details.imageurl">> => NewUserDetails#user_details.imageurl
            }),
            Command = #{<<"$set">> => UserDetails},
            mc_worker_api:update(Connection, <<"details">>, #{<<"_id">> => UserID}, Command),

            mc_worker_api:disconnect(Connection),
            {ok, updated};
        _ -> {error, database}
    end;
user_set_details_by_userid(_, {error, Reason}, _) ->
    {error, Reason}.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Log out the user with the given session id.
% ----------------------------------------------------------------------------------------------------
user_logout(_, SessionID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            % Grab all the sessions with this ID
            case mc_worker_api:delete(Connection, <<"sessions">>, #{<<"_id">> => SessionID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, sessionid};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, logged_out}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Remove all the sessions for a given user that are older than the given number of hours
% ----------------------------------------------------------------------------------------------------
user_remove_all_sessions(Connection, UserID) -> user_remove_all_sessions(Connection, UserID, 0).
user_remove_all_sessions(_Connection, UserID, NumHours) ->
    % Grab all the sessionIDs for this UserID
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"sessions">>, #{<<"userid">> => UserID}) of
                {ok, Cursor} ->
                    SessionList = get_all_sessions(Cursor),
                    mc_cursor:close(Cursor),
                    Result = delete_all_sessions(Connection2, SessionList, NumHours),
                    mc_worker_api:disconnect(Connection2),
                    Result;
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, done}
            end;
        _ -> {error, database}
    end.

% Extract just the SessionIDs - we could just delete them here, but not sure what happens
% if deleting an entry that is also now part of a cursor find - safer this way.
get_all_sessions(Cursor) -> get_sessions_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_sessions_at_cursor(_, error) -> [];
get_sessions_at_cursor(_, {}) -> [];
get_sessions_at_cursor(Cursor, {SessionMap}) ->
    [{maps:get(<<"_id">>, SessionMap), maps:get(<<"created">>, SessionMap)} | get_sessions_at_cursor(Cursor, mc_cursor:next(Cursor))].

% Delete the Sessions in the list if expired
delete_all_sessions(_, [], _) -> {ok, done};
delete_all_sessions(Connection, [{SessionID, CreatedBinary} | SessionIDs], NumHours) ->
    Created = iso8601:parse(CreatedBinary),
    % Work out the time difference in seconds
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds(Created),
    if
        (TimeDiff >= (NumHours * 3600)) ->
            % Delete it if expired
            mc_worker_api:delete(Connection, <<"sessions">>, #{<<"_id">> => SessionID});
        true -> ok
    end,
    delete_all_sessions(Connection, SessionIDs, NumHours).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the given user, but only if we can confirm with useremail and password
% ----------------------------------------------------------------------------------------------------
user_delete(Connection, SessionID, UserID) ->
    case user_id_from_sessionid(Connection, SessionID) of
        % Check this matches the UserID parameter
        {ok, UserID} ->
            % Delete all the Channels and Geobots of this user
            case imersia_db:channel_delete_all(Connection, UserID) of
                {ok, deleted} ->
                    % Remove any sessions
                    case user_remove_all_sessions(Connection, UserID) of
                        {ok, done} ->
                            % Remove the entry in the Companions Collection
                            {ok, Connection2} = mc_worker_api:connect ([{database, <<"imersia_admin">>}]),
                            mc_worker_api:delete(Connection2, <<"companions">>, #{<<"_id">> => UserID}),
                            mc_worker_api:disconnect(Connection2),

                            % Drop the user's database
                            {ok, Connection3} = mc_worker_api:connect ([{database, <<"imersia_", UserID/binary>>}]),
                            mc_worker_api:command(Connection3, #{<<"dropDatabase">> => 1}),
                            mc_worker_api:disconnect(Connection3),

                            {ok, deleted};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {ok, _} -> {error, userid};
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check that this session exists and is valid for this user, and update if necessary
% ----------------------------------------------------------------------------------------------------
user_session_exists(_, SessionID, UserID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            % Grab all the sessions with this ID
            case mc_worker_api:find_one(Connection, <<"sessions">>, #{<<"_id">> => SessionID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, sessionid};
                Session ->
                    SessionUserID = maps:get(<<"userid">>, Session),
                    if
                        UserID =:= SessionUserID ->
                            case delete_or_renew_session(Connection, Session) of
                                {ok, updated} ->
                                    mc_worker_api:disconnect(Connection),
                                    {ok, exists};
                                _ ->
                                    mc_worker_api:disconnect(Connection),
                                    {error, sessionid}
                            end;
                        true ->
                            mc_worker_api:disconnect(Connection),
                            {error, userid}
                    end
            end;
        _ ->
            {error, database}
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

    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            CreatedTime = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - (Age * 3600)),
            SessionID = imersia_db:new_id(),
            DBEntry = #session {
                token = Token,
                userid = UserID,
                created = iso8601:format(CreatedTime)
            },
            DBEntryMap = maps:remove(<<"sessionid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

            mc_worker_api:insert(Connection2, <<"sessions">>, DBEntryMap#{<<"_id">> => SessionID}),
            mc_worker_api:disconnect(Connection2),
            {ok, SessionID};
        _ ->
            {error, database}
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

    % Update database entry for this Companion / User
    case mc_worker_api:connect ([{database, <<"imersia_", UserID/binary>>}]) of
        {ok, Connection2} ->
            Command = #{<<"$set">> => #{
                <<"modified">> => iso8601:format(calendar:universal_time()),
                <<"password">> => EncryptedPassword
            }},
            case mc_worker_api:update(Connection2, <<"details">>, #{<<"_id">> => UserID}, Command) of
                {true, _} ->
                    user_remove_all_sessions(Connection, UserID),
                    mc_worker_api:disconnect(Connection2),
                    {ok, updated};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {error, database}
            end;
        _ ->
            {error, database}
    end;

user_change_password_by_userid(_, {error, Reason}, _) ->
    {error, Reason}.
% ----------------------------------------------------------------------------------------------------
