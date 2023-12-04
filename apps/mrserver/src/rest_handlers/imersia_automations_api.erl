% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Automations Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_automations_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2,
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
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Collect common parameters
	Connection = imersia_db:connect(),
	SessionID = cowboy_req:header(<<"sessionid">>, Req),
	GeobotID = cowboy_req:header(<<"geobotid">>, Req),
    AutomationID = cowboy_req:header(<<"automationid">>, Req),

    % Get the User's ID from the SessionID
	UserID = get_userid(Connection, SessionID),

    % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, sessionid => SessionID, userid => UserID, geobotid => GeobotID, automationid => AutomationID},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
    Location = cowboy_req:header(<<"location">>, Req2),
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} ->
            Allowance = check_authorization(Connection, cowboy_req:method(Req), GeobotID, UserID, ContextIDs),
            {Parameters2, Response} = case Allowance of
                {ok, Action} ->
                    { Parameters#{action => Action}, true };
                {error, Reason} ->
                    imersia_db:close(Connection),
                    { Parameters, {false, atom_to_list(Reason)} }
            end,
            Req3 = Req2#{parameters => Parameters2},
            {Response, Req3, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {{false, atom_to_list(Reason)} , Req2, State}
    end.

check_authorization(Connection, <<"POST">>, GeobotID, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, <<"PUT">>, GeobotID, undefined, UserID, ContextIDs);
check_authorization(Connection, <<"DELETE">>, GeobotID, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, <<"PUT">>, GeobotID, undefined, UserID, ContextIDs);
check_authorization(Connection, Method, GeobotID, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, Method, GeobotID, undefined, UserID, ContextIDs).

% Get the UserID from the SessionID
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the GeobotID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    case do_check_developerid(Req, State) of
        {true, _, _} -> do_resource_exists(cowboy_req:method(Req), Req, State);
        Error -> Error
    end.

do_resource_exists(<<"HEAD">>, Req, State) ->
    #{parameters := #{connection:= Connection}} = Req,
    imersia_db:close(Connection),
    {true, Req, State};

do_resource_exists(<<"DELETE">>, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection:= Connection, automationid := AutomationID}} = Req,
    case AutomationID of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(automationid, Req, State);
        _ -> {true, Req, State}
    end;

do_resource_exists(<<"GET">>, Req, State) ->
    #{parameters := #{connection:= Connection, automationid := AutomationID, geobotid := GeobotID}} = Req,
    case AutomationID of
        undefined -> {true, Req, State};  % Get list of automations
        _ ->
            case imersia_db:automation_exists(Connection, GeobotID, AutomationID) of
                {ok, exists} -> {true, Req, State};
                {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
            end
    end;

do_resource_exists(_, Req, State) -> {true, Req, State}.

do_check_developerid(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{userid := UserID, connection := Connection, geobotid := GeobotID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    Method = cowboy_req:method(Req),
    if
        (UserID == undefined) and (Method /= <<"GET">>) -> imersia_misc:response_error(sessionid, Req, State);
        true ->
			case imersia_db:geobot_exists(Connection, GeobotID) of
                {ok, exists} -> {true, Req, State};
                {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
            end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, geobotid := GeobotID, automationid := AutomationID}} = Req,
    % Get the automation value
    get_automation(Connection, GeobotID, AutomationID, Req, State).

% No AutomationID defined, so send a list of all Automations
get_automation(Connection, GeobotID, undefined, Req, State) ->
    case imersia_db:automation_list(Connection, GeobotID) of
        {ok, AutomationList} ->
            {imersia_misc:safely_encode_json(convert_automations_to_json(AutomationList)), Req, State};
        {error, Reason} -> {jsx:encode([{error, Reason}]), Req, State}
    end;
% Valid AutomationID
get_automation(Connection, GeobotID, AutomationID, Req, State) ->
    case imersia_db:automation_getdetails(Connection, GeobotID, AutomationID) of
        {ok, Automation} ->
            AutomationJSON = imersia_misc:record_to_json(Automation, true),
            {imersia_misc:safely_encode_json(AutomationJSON), Req, State};
        {error, Reason} -> {jsx:encode([{error, Reason}]), Req, State}
    end.

convert_automations_to_json([]) -> [];
convert_automations_to_json([Automation | Automations]) ->
    [imersia_misc:record_to_json(Automation, true) | convert_automations_to_json(Automations)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, geobotid := GeobotID, userid := UserID, automationid := AutomationIDFromHeader}} = Req,

    % Grab the new or updated Automation details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(automation, Body) of
        error -> imersia_misc:response_error(automation, Req, State);
        Automation ->
            AutomationID = one_or_other(AutomationIDFromHeader, Automation#automation.automationid),
            case UserID of
                % The user has to be logged in
                undefined -> imersia_misc:response_error(sessionid, Req, State);

                % Either create a new Automation or update an existing one
                _ -> set_automation(Connection, GeobotID, AutomationID, Automation, Req, State)
            end
    end.

% Set the Automation either using the existing ID, or as a new Automation
set_automation(Connection, GeobotID, undefined, Automation, Req, State) ->
    case imersia_db:automation_new(Connection, GeobotID, Automation) of
        {ok, AutomationID} -> {true, cowboy_req:set_resp_body(jsx:encode([{ok, updated}, {automationid, AutomationID}]), Req), State};
        {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
    end;
set_automation(Connection, GeobotID, AutomationID, Automation, Req, State) ->
    case imersia_db:automation_setdetails(Connection, GeobotID, AutomationID, Automation) of
        {ok, _} -> {true, cowboy_req:set_resp_body(jsx:encode([{ok, updated}, {automationid, AutomationID}]), Req), State};
        {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
    end.


% Choose the one that is not undefined, preferring the one in the header
one_or_other(A, <<>>) -> A;
one_or_other(A, null) -> A;
one_or_other(undefined, B) -> B;
one_or_other(A, _) -> A.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete an Automation
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters
    #{parameters := #{connection := Connection, automationid := AutomationID, geobotid := GeobotID}} = Req,
    % Delete the Automation
    case imersia_db:automation_delete(Connection, GeobotID, AutomationID) of
        {ok, deleted} -> {true, cowboy_req:set_resp_body(jsx:encode([{ok, deleted}]), Req), State};
        {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
    end.
delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
