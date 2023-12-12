% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Analytics Handler for the API
% ----------------------------------------------------------------------------------------------------
-module(imersia_analytics_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2,
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
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.
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
	ID = imersia_misc:id_type(cowboy_req:header(<<"channelid">>, Req), cowboy_req:header(<<"geobotid">>, Req), cowboy_req:header(<<"userid">>, Req)),
    DeveloperID = cowboy_req:header(<<"developerid">>, Req),
    Location = cowboy_req:header(<<"location">>, Req),

    % For GET
    Event = cowboy_req:header(<<"event">>, Req),
    Startdate = cowboy_req:header(<<"startdate">>, Req),
    Enddate = cowboy_req:header(<<"enddate">>, Req),

    % Get the UsersID
	UserID = get_userid(Connection, SessionID),

    % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, geohash => Location, sessionid => SessionID, userid => UserID, id => ID, getparams => {Event, Startdate, Enddate}},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
        {ok, logged} ->
            Allowance = check_authorization(Connection, cowboy_req:method(Req), ID, UserID, ContextIDs),
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

check_authorization(Connection, <<"POST">>, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, <<"PUT">>, ChannelID, UserID, ContextIDs);
check_authorization(Connection, Method, {channelid, ChannelID}, UserID, ContextIDs) ->
    imersia_auth:check_channel(Connection, Method, ChannelID, UserID, ContextIDs);

check_authorization(Connection, <<"POST">>, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, <<"PUT">>, GeobotID, undefined, UserID, ContextIDs);
check_authorization(Connection, Method, {geobotid, GeobotID}, UserID, ContextIDs) ->
    imersia_auth:check_geobot(Connection, Method, GeobotID, undefined, UserID, ContextIDs);

check_authorization(Connection, Method, {userid, UserID}, UserID, _) ->
    imersia_auth:check_user(Connection, Method, UserID);

check_authorization(_, _, _, _, _) -> {error, id}.

% Get the UserID from the SessionID
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the item ID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    case do_check_developerid(Req, State) of
        {true, _, _} -> {true, Req, State};
        Error -> Error
    end.

do_check_developerid(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{userid := UserID, connection := Connection, id := ID}} = Req,

    % Respond with an error where appropriate (mostly applicable for GET)
    if
        (UserID == undefined) -> imersia_misc:response_error(sessionid, Req, State);
        true ->
			case check_existance(Connection, ID, UserID) of
                {ok, exists} -> {true, Req, State};
                {error, Reason} -> imersia_misc:response_error(Reason, Req, State)
            end
    end.

check_existance(Connection, {channelid, ChannelID}, _) -> imersia_db:channel_exists(Connection, ChannelID);
check_existance(Connection, {geobotid, GeobotID}, _) -> imersia_db:geobot_exists(Connection, GeobotID);
check_existance(_, {userid, UserID}, UserID) -> {ok, exists};
check_existance(_, {userid, _}, _) -> {error, id};
check_existance(_, _, _) -> {error, id}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, id := {_, GeneralID}, getparams := {Event, Startdate, Enddate}}} = Req,

    % Convert the start and end dates into standard format.  The input format is YYYYMMDD (in Decimal).
    % And then fill in the blanks.
    {StartdateConverted, EnddateConverted} = augment_dates(convert_date(Startdate), convert_date(Enddate)),

    % Get the Analytics
    case imersia_db:analytics_query(Connection, GeneralID, Event, StartdateConverted, EnddateConverted) of
        {ok, AnalyticsList} ->
            imersia_db:close(Connection),
            {imersia_misc:safely_encode_json(convert_analytics_to_json(AnalyticsList)), Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {jsx:encode([{error, Reason}]), Req, State}
    end.

convert_date(<<Year:4/binary, Month:2/binary, Day:2/binary>>) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, {0, 0, 0}};
convert_date(_) -> undefined.

% Undefined dates - choose from a month ago to today
augment_dates(undefined, undefined) ->
    {Datepart, Timepart} = calendar:universal_time(),
    {{edate:shift(Datepart, -1, months), Timepart}, {Datepart, Timepart}};
% Undefined startdate - choose a month back from the given enddate
augment_dates(undefined, Enddate) ->
    {Datepart, Timepart} = Enddate,
    {{edate:shift(Datepart, -1, months), Timepart}, Enddate};
% Undefined enddate - use today (now)
augment_dates(Startdate, undefined) ->
    Today = calendar:universal_time(),
    {Startdate, Today};
augment_dates(Startdate, Enddate) -> {Startdate, Enddate}.

convert_analytics_to_json([]) -> [];
convert_analytics_to_json([Head | Tail]) ->
    <<GeohashPart:8/binary, _/binary>> = Head#analytic.location#location.geohash,
    Analytic = [
        {<<"created">>, Head#analytic.created},
        {<<"geohash">>, GeohashPart},
        {<<"event">>, Head#analytic.event},
        {<<"tally">>, Head#analytic.tally},
        {<<"parameters">>, imersia_misc:safely_decode_json(Head#analytic.params, return_value)},
        {<<"indexes">>, imersia_misc:safely_decode_json(Head#analytic.indexes, return_value)}
    ],
    [Analytic | convert_analytics_to_json(Tail)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{geohash := Location, connection := Connection, userid := UserID, id := {_, GeneralID}}} = Req,

    % Grab the new analytics details
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),

    % Grab the event and parameters in particular, all else is ignored
    Event = ej:get({<<"event">>}, Body),
    Parameters = ej:get({<<"parameters">>}, Body),
    Date = calendar:universal_time(),

    case Event of
        undefined ->
            imersia_db:close(Connection),
            imersia_misc:response_error(event, Req, State);
        _ ->
            case UserID of
                % The user has to be logged in
                undefined ->
                    imersia_db:close(Connection),
                    imersia_misc:response_error(sessionid, Req, State);

                % Create a new analytics entry or update an existing one
                _ ->
                    case imersia_db:analytics_log(Connection, GeneralID, Event, Location, Date, Parameters) of
                        {ok, AnalyticID} ->
                            imersia_db:close(Connection),
                            {true, cowboy_req:set_resp_body(jsx:encode([{analyticid, AnalyticID}]), Req), State};
                        {error, Reason} ->
                            imersia_db:close(Connection),
                            imersia_misc:response_error(Reason, Req, State)
                    end
            end
    end.
% ----------------------------------------------------------------------------------------------------
