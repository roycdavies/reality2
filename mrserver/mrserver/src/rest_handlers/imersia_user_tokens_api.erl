% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The User Handler for the API
% GET       - Get a user's number of tokens and transactions
% POST      -
% PUT       -
% HEAD      -
% OPTIONS   - Returns the allowed methods (GET, HEAD, OPTIONS)
% DELETE    -
%
% TODO: Check and Log the DeveloperID and Location details
% TODO: Set / User Cookies
% ----------------------------------------------------------------------------------------------------
-module(imersia_user_tokens_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2,
    is_authorized/2,
    resource_exists/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check Authorization
% TODO: Allow only certain users to create new users
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> {true, Req, State};
        _ ->
            % Get all the parameters
            Connection = imersia_db:connect(),
            SessionID = cowboy_req:header(<<"sessionid">>, Req),
            UserID = get_user_id(Connection, SessionID, cowboy_req:binding(userid, Req), cowboy_req:header(<<"userid">>, Req)),

            % Continue if this is a valid command for this developerID
            DeveloperID = cowboy_req:header(<<"developerid">>, Req),
            Location = cowboy_req:header(<<"location">>, Req),

            case imersia_db:user_email(Connection, UserID) of
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {{false, atom_to_list(Reason)}, Req, State};
                {ok, UserEmail} ->
                    % Add these to the stream for later
                    Parameters = #{useremail => UserEmail, sessionid => SessionID, userid => UserID, connection => Connection, location => Location},
                    Req2 = Req#{parameters => Parameters},

                    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
                        {ok, logged} -> {true, Req2, State};
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            {{false, atom_to_list(Reason)} , Req2, State}
                    end
            end
    end.

get_user_id(_, undefined, undefined, undefined) -> undefined;
get_user_id(Connection, SessionID, undefined, undefined) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end;
get_user_id(_, _, undefined, UserID) -> UserID;
get_user_id(_, _, UserID, undefined) -> UserID.
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
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, useremail := UserEmail}} = Req,

    case cowboy_req:method(Req) of
        <<"GET">> ->
            case imersia_billing:get_num_tokens(UserEmail) of
                {ok, NumTokens} ->
                    imersia_db:close(Connection),
                    {imersia_misc:safely_encode_json([{tokens, NumTokens}]), Req, State};
                {error, _} ->
                    imersia_db:close(Connection),
                    {imersia_misc:safely_encode_json([{error, no_tokens}]) , Req, State}
            end;
        <<"HEAD">> ->
            imersia_db:close(Connection),
            {"", Req, State}
    end.
% ----------------------------------------------------------------------------------------------------
