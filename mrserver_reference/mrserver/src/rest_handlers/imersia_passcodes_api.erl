% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: API for getting a passcode
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_passcodes_api).

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_json/2,
    options/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"GET">>, <<"OPTIONS">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Preflight CORS parameters
% ----------------------------------------------------------------------------------------------------
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"developerid, location, useremail">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"$*">>, Req2),
    {ok, Req3, State}.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
	Connection = imersia_db:connect(),
    UserEmail = cowboy_req:header(<<"useremail">>, Req),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, useremail => UserEmail},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
    Location = cowboy_req:header(<<"location">>, Req2),
    case UserEmail of
        undefined -> {{false, "useremail"} , Req2, State};
        _ ->
            case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
                {ok, logged} ->
                    {true, Req2, State};
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {{false, atom_to_list(Reason)} , Req2, State}
            end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET - either a passcode is created, or there was an error.
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    #{parameters := #{useremail := UserEmail, connection := Connection}} = Req,
    case imersia_db:passcode_new(Connection, UserEmail) of
        {ok, created} -> imersia_db:close(Connection), {jsx:encode([{ok, sent}]), Req, State};
        {error, Reason} -> imersia_db:close(Connection), {jsx:encode([{error, Reason}]), Req, State}
    end.
% ----------------------------------------------------------------------------------------------------
