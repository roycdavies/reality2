% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Super Admin commands
% GET       - Get super admin details about stuff on the MRServer
% POST      -
% PUT       -
% HEAD      -
% OPTIONS   - Returns the allowed methods (GET, HEAD, OPTIONS, DELETE, POST, PUT)
% DELETE    -
% ----------------------------------------------------------------------------------------------------
-module(imersia_admin_api).

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
% The user has to be both logged in, and be the super admin user in the settings file.
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Get all the parameters
    Connection = imersia_db:connect(),
    SessionID = cowboy_req:header(<<"sessionid">>, Req),
    DeveloperID = cowboy_req:header(<<"developerid">>, Req),
    Location = cowboy_req:header(<<"location">>, Req),
    Command = cowboy_req:header(<<"command">>, Req),
    Params = cowboy_req:header(<<"parameters">>, Req),

    % If there is no superadmin account, fail immediately
    case imersia_settings:get_setting(superadmin, account) of
        undefined ->
            imersia_db:close(Connection),
            {{false, "account"} , Req, State};
        SuperAdminEmail ->
            % Get the exception code word in case this is to be circumvented.  The word in the mrserver.toml file has to match the one in the makefile
            SuperAdminException = case imersia_settings:get_setting(mrserver, admin_exception) of
                undefined -> undefined;
                Val -> string:to_lower(binary_to_list(Val))
            end,
            case imersia_db:user_get_details(Connection, SessionID) of
                {error, Reason} ->
                    imersia_db:close(Connection),
                    {{false, atom_to_list(Reason)}, Req, State};
                {ok, User} ->
                    case test_admin_user(string:to_lower(binary_to_list(User#user.useremail)), string:to_lower(binary_to_list(SuperAdminEmail)), SuperAdminException) of
                        error ->
                            imersia_db:close(Connection),
                            {{false, "account"} , Req, State};
                        ok ->
                            % Add these to the stream for later
                            Parameters = #{ sessionid => SessionID, command => Command, parameters => Params, connection => Connection, userid => User#user.userid, useremail => User#user.useremail, location => Location},
                            Req2 = Req#{parameters => Parameters},

                            % Continue if this is a valid command for this developerID
                            case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
                                {ok, logged} -> {true, Req2, State};
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    {{false, atom_to_list(Reason)} , Req2, State}
                            end
                    end
            end
    end.

test_admin_user(_, undefined, _) -> error;
test_admin_user(_, SuperAdminException, SuperAdminException) -> ok;
test_admin_user(UserEmail, UserEmail, _) -> ok;
test_admin_user(_, _, _) -> error.
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
        {error, Reason} -> imersia_db:close(Connection), imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete something
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, userid := _UserID, sessionid := _SessionID}} = Req,

    Req0 = cowboy_req:set_resp_body(<<"">>, Req),
    imersia_db:close(Connection),
    {true, Req0, State}.

delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, sessionid := _SessionID, command := Command, parameters := Params}} = Req,

    case cowboy_req:method(Req) of
        <<"GET">> ->
            ParametersJSON = imersia_misc:safely_decode_json(Params),
            Response = imersia_misc:safely_encode_json(process_command(Connection, Command, ParametersJSON)),
            imersia_db:close(Connection),
            {Response, Req, State};
        <<"HEAD">> ->
            imersia_db:close(Connection),
            {"", Req, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Process the command sent in
% ----------------------------------------------------------------------------------------------------
process_command(Connection, <<"stats">>, _) ->
    build_stats_response(
        imersia_db:user_list(Connection),
        imersia_db:channel_list(Connection),
        imersia_db:geobot_list(Connection)
    );
process_command(Connection, <<"users">>, _) ->
    build_users_response(Connection, imersia_db:user_list(Connection));

process_command(Connection, <<"channels">>, [{<<"userid">>, UserID}]) ->
    build_channels_response(imersia_db:channel_list(Connection, UserID, UserID, true));
process_command(_, <<"channels">>, _) -> [{}];

process_command(Connection, <<"geobots">>, Parameters) ->
    UserID = ej:get({"userid"}, Parameters),
    ChannelID = ej:get({"channelid"}, Parameters),
    build_geobots_response(imersia_db:geobot_list(Connection, ChannelID, UserID, true));

process_command(Connection, <<"metadata">>, [{_, ID}]) ->
    build_metadata_response(imersia_db:metadata_list(Connection, ID));

process_command(Connection, <<"analytics">>, Parameters) ->
    GeobotID = ej:get({"geobotid"}, Parameters),
    UserID = ej:get({"userid"}, Parameters),
    Startdate = ej:get({"startdate"}, Parameters),
    Enddate = ej:get({"enddate"}, Parameters),
    Event = ej:get({"event"}, Parameters),
    build_analytics_response(Connection, UserID, GeobotID, Startdate, Enddate, Event);

process_command(Connection, <<"automations">>, Parameters) ->
    GeobotID = ej:get({"geobotid"}, Parameters),
    build_automations_response(Connection, GeobotID);

process_command(_, _, _) -> [{error, command}].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Count the User, Channels and Geobots on this MRServer
% ----------------------------------------------------------------------------------------------------
build_stats_response({ok, UserIDs}, {ok, ChannelIDs}, {ok, GeobotIDs}) ->
    [{<<"userids">>, UserIDs}, {<<"channelids">>, ChannelIDs}, {<<"geobotids">>, GeobotIDs}];
build_stats_response(_, _, _) -> [{error, database}].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List of users
% ----------------------------------------------------------------------------------------------------
build_users_response(_, {ok, []}) -> [];
build_users_response(_, {error, _}) -> [{error, database}];
build_users_response(Connection, {ok, [UserID | UserIDs]}) ->
    case (imersia_db:user_get_details_by_userid(Connection, UserID)) of
        {ok, UserDetails} -> [imersia_misc:record_to_json(UserDetails, false) | build_users_response(Connection, {ok, UserIDs})];
        {error, _} -> build_users_response(Connection, {ok, UserIDs})
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% User's channels
% ----------------------------------------------------------------------------------------------------
build_channels_response({error, Reason}) -> [{error, Reason}];
build_channels_response({ok, Channels}) -> convert_channels(Channels).

convert_channels([]) -> [];
convert_channels([Channel | Channels]) -> [imersia_misc:record_to_json(Channel, false) | convert_channels(Channels)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Geobots on a Channel
% ----------------------------------------------------------------------------------------------------
build_geobots_response({error, Reason}) -> [{error, Reason}];
build_geobots_response({ok, Geobots}) -> convert_geobots(Geobots).

convert_geobots([]) -> [];
convert_geobots([Geobot | Geobots]) -> [imersia_misc:record_to_json(Geobot, false) | convert_geobots(Geobots)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Metadata
% ----------------------------------------------------------------------------------------------------
build_metadata_response({error, Reason}) -> [{error, Reason}];
build_metadata_response({ok, Metadatas}) -> convert_metadatas(Metadatas).

convert_metadatas([]) -> [];
convert_metadatas([Metadata | Metadatas]) -> [imersia_misc:record_to_json(Metadata, false) | convert_metadatas(Metadatas)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Analytics
% ----------------------------------------------------------------------------------------------------
build_analytics_response(_, _, undefined, _, _, _) -> [{error, geobotid}];
build_analytics_response(Connection, _UserID, GeobotID, Startdate, Enddate, Event) ->
    % Convert the start and end dates into standard format.  The input format is YYYYMMDD (in Decimal).
    % And then fill in the blanks.
    {StartdateConverted, EnddateConverted} = augment_dates(convert_date(Startdate), convert_date(Enddate)),

    % Get the Analytics
    case imersia_db:analytics_query(Connection, GeobotID, Event, StartdateConverted, EnddateConverted) of
        {ok, AnalyticsList} ->
            convert_analytics_to_json(AnalyticsList);
        {error, Reason} -> [{error, Reason}]
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
% Automations
% ----------------------------------------------------------------------------------------------------
build_automations_response(_, undefined) -> [{error, geobotid}];
build_automations_response(Connection, GeobotID) ->
    case imersia_db:automation_list(Connection, GeobotID) of
        {ok, AutomationList} -> convert_automations_to_json(AutomationList);
        {error, Reason} -> [{error, Reason}]
    end.

convert_automations_to_json([]) -> [];
convert_automations_to_json([Automation | Automations]) ->
    [imersia_misc:record_to_json(Automation, true) | convert_automations_to_json(Automations)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) -> action_from_json(Req, State).

% A POST or PUT Session
action_from_json(Req, State) ->
    % Grab the parameters passed in
    #{parameters := #{connection := Connection, sessionid := _SessionID, location := _Location, command := Command, parameters := Params}} = Req,

    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),


    ParametersJSON = imersia_misc:safely_decode_json(Params),
    case process_command(Connection, Command, ParametersJSON, Body) of
        {ok, Response} ->
            imersia_db:close(Connection),
            erlang:display(Response),
            {true, cowboy_req:set_resp_body(Response, Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Process the command sent in as POST or PUT
% ----------------------------------------------------------------------------------------------------
process_command(Connection, <<"lock">>, Parameters, _) ->
    ID = imersia_misc:id_type(ej:get({<<"channelid">>}, Parameters), ej:get({<<"geobotid">>}, Parameters), ej:get({<<"userid">>}, Parameters)),
    do_lock_command(Connection, ID);

process_command(Connection, <<"unlock">>, Parameters, _) ->
    ID = imersia_misc:id_type(ej:get({<<"channelid">>}, Parameters), ej:get({<<"geobotid">>}, Parameters), ej:get({<<"userid">>}, Parameters)),
    do_unlock_command(Connection, ID);

process_command(_, _, _, _) -> {error, command}.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
do_lock_command(_, undefined) -> {error, id};
do_lock_command(Connection, {geobotid, GeobotID}) ->
    GeobotDetails = #geobot{
        hidden = true
    },
    case imersia_db:geobot_setdetails(Connection, GeobotID, GeobotDetails) of
        {ok, updated} -> {ok, jsx:encode([{ok, locked}])};
        {error, Reason} -> {error, Reason}
    end;

do_lock_command(Connection, {channelid, ChannelID}) ->
    ChannelDetails = #channel{
        hidden = true
    },
    case imersia_db:channel_setdetails(Connection, ChannelID, ChannelDetails) of
        {ok, updated} -> {ok, jsx:encode([{ok, locked}])};
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
do_unlock_command(_, undefined) -> {error, id};
do_unlock_command(Connection, {geobotid, GeobotID}) ->
    GeobotDetails = #geobot{
        hidden = false
    },
    case imersia_db:geobot_setdetails(Connection, GeobotID, GeobotDetails) of
        {ok, updated} -> {ok, jsx:encode([{ok, unlocked}])};
        {error, Reason} -> {error, Reason}
    end;

do_unlock_command(Connection, {channelid, ChannelID}) ->
    ChannelDetails = #channel{
        hidden = false
    },
    case imersia_db:channel_setdetails(Connection, ChannelID, ChannelDetails) of
        {ok, updated} -> {ok, jsx:encode([{ok, unlocked}])};
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
