% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Unit tests for the API.  To use these ones, you need to run the MR Server first (make run) and then in
% the CLI type test_api:test().
% ----------------------------------------------------------------------------------------------------
-module(test_api).

-export([test/0]).

-include_lib ("etest_http/include/etest_http.hrl").
-include("../imersia_datatypes.hrl").

-define (POST_TYPE, {<<"Content-Type">>, <<"application/json; charset=utf-8">>}).

test() ->
    DeveloperID = <<"Test">>,
    Location = <<"rckq2f79wt0p">>,

    Connection = imersia_db:connect(),
    UserEmail = <<"fred2@dagg.co.nz">>,
    AnotherPass = <<"12345">>,
    Details = #user_details{
        firstname = <<"Fred">>,
        surname = <<"Dagg">>,
        nickname = <<"A Good Keen Man">>
    },
    NewDetails = imersia_misc:record_to_json(#user_details{
        firstname = <<"John">>
    }, true),
    UserPass = <<"password">>,
    Token = '1234567890',
    WrongPass = <<"HelloWorld">>,
    WrongUser = <<"here@now">>,
    WrongSessionID = imersia_db:new_id(),

    % Make sure the test user isn't hanging around from a previous test
    ok = delete_user_if_exists(Connection, UserEmail),

    % Create a new user for testing purposes
    UserDetails = imersia_misc:record_to_json(#user{
        useremail = UserEmail,
        password = UserPass,
        details = Details
    }, true),

    % ----------------------------------------------------------------------------------------------------
    % Test Users and Sessions
    % ----------------------------------------------------------------------------------------------------

    % Create a new user for testing purposes
    Response85 = ?perform_post("https://localhost/api/user", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}], imersia_misc:safely_encode_json(UserDetails)),
    ?assert_status(200, Response85),

    % Extract the UserID
    {_, _, _, SessionBodyJSON0} = Response85,
    UserID = ej:get({"userid"}, jiffy:decode(SessionBodyJSON0)),

    % Try creating a user with the same UserID
    Response87 = ?perform_post("https://localhost/api/user", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}], imersia_misc:safely_encode_json(UserDetails)),
    ?assert_status(400, Response87),

    % Logging in without a token
    Response0 = ?perform_get("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {password, UserPass}]),
    ?assert_status(200, Response0),

    % Grab the SessionID
    {_, _, _, SessionBodyJSON} = Response0,
    SessionID = ej:get({"sessionid"}, jiffy:decode(SessionBodyJSON)),

    Response90 = ?perform_put("https://localhost/api/user/" ++ binary_to_list(UserID), [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID}], imersia_misc:safely_encode_json(NewDetails)),
    ?assert_status(200, Response90),

    % Logging in with a token
    Response1 = ?perform_get("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {password, UserPass}, {token, Token}]),
    ?assert_status(200, Response1),

    % Logging in with incorrect details
    Response2 = ?perform_get("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {password, WrongPass}]),
    ?assert_status(404, Response2),

    % Logging in with incorrect details
    Response3 = ?perform_get("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {useremail, WrongUser}, {password, UserPass}]),
    ?assert_status(404, Response3),

    % Change the password
    erlang:display(UserID),
    erlang:display(SessionID),
    erlang:display(DeveloperID),
    erlang:display(Location),

    Response30 = ?perform_put("https://localhost/api/user/password", [?POST_TYPE, {userid, UserID}, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID}, {password, AnotherPass}]),
    erlang:display(Response30),
    ?assert_status(200, Response30),

    % Login with new password
    Response31 = ?perform_get("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {password, AnotherPass}, {token, Token}]),
    ?assert_status(200, Response31),

    % Grab the SessionID
    {_, _, _, SessionBodyJSON3} = Response31,
    SessionID3 = ej:get({"sessionid"}, jiffy:decode(SessionBodyJSON3)),

    % Get user details after logging in, but using the old SessionID - changing the password deletes all the sessionIDs for that user
    Response80 = ?perform_get("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {sessionid, SessionID}]),
    ?assert_status(404, Response80),

    % Get user details after logging in, but using the old SessionID
    Response91 = ?perform_get("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}, {sessionid, SessionID3}]),
    ?assert_status(200, Response91),

    % Get user details by logging in with wrong credentials
    Response81 = ?perform_get("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {useremail, WrongUser}, {sessionid, SessionID3}]),
    ?assert_status(404, Response81),

    % Get user details by logging in with right email address, but no sessionid
    Response82 = ?perform_get("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}]),
    ?assert_status(404, Response82),

    % Delete the Session
    Response83 = ?perform_delete("https://localhost/api/sessions/" ++ binary_to_list(SessionID3), [{developerid, DeveloperID}, {location, Location}, {useremail, UserEmail}]),
    ?assert_status(204, Response83),

    % Try deleting the session, but it has already gone (or doesn't exist).  Also tests sending sessionID via header rather than URL.
    Response84 = ?perform_delete("https://localhost/api/sessions", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID3}, {useremail, UserEmail}]),
    ?assert_status(404, Response84),

    % Try deleting the user with wrong credentials
    Response86 = ?perform_delete("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {userid, UserID}, {sessionid, WrongSessionID}]),
    ?assert_status(404, Response86),

    % Try deleting the user with an egeobotired SessionID
    Response88 = ?perform_delete("https://localhost/api/user", [{developerid, DeveloperID}, {location, Location}, {userid, UserID}, {sessionid, SessionID}]),
    ?assert_status(404, Response88),

    % Response89 = ?perform_delete("https://localhost/api/user", [{userid, UserID}, {sessionid, SessionID2}]),
    % ?assert_status(200, Response89),

    % ----------------------------------------------------------------------------------------------------
    % Test Channels
    % ----------------------------------------------------------------------------------------------------
    {ok, SessionID2} = imersia_db:user_session_new(Connection, Token, UserEmail),

    ChannelName = <<"Roys_Channel">>,
    ChannelName2 = <<"Roys_Other_Channel">>,
    ChannelDescription = <<"This is my first channel">>,
    AChannel = imersia_misc:record_to_json(#channel{
        name = ChannelName,
        description = ChannelDescription
    }, true),
    AnotherChannel = imersia_misc:record_to_json(#channel{
        name = ChannelName2,
        description = ChannelDescription
    }, true),
    _WrongChannelID = imersia_db:new_id(),

    ResponseCH1 = ?perform_post("https://localhost/api/channels", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}], imersia_misc:safely_encode_json(AChannel)),
    ?assert_status(200, ResponseCH1),
    % Grab the ChannelID
    {_, _, _, ChannelBodyJSON1} = ResponseCH1,
    ChannelID1 = ej:get({"channelid"}, jiffy:decode(ChannelBodyJSON1)),

    ResponseCH2 = ?perform_get("https://localhost/api/channels/" ++ binary_to_list(ChannelName), [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}]),
    ?assert_status(200, ResponseCH2),

    % Check the values returned are the same as those sent in
    {_, _, _, ChannelBodyJSON2} = ResponseCH2,
    DecodedChannelBodyJSON2 = jiffy:decode(ChannelBodyJSON2),
    ChannelID1 = ej:get({"channelid"}, DecodedChannelBodyJSON2),
    ChannelName = ej:get({"name"}, DecodedChannelBodyJSON2),
    ChannelDescription = ej:get({"description"}, DecodedChannelBodyJSON2),

    ResponseCH3 = ?perform_get("https://localhost/api/channels", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {channelid, ChannelID1}]),
    ?assert_status(200, ResponseCH3),

    ResponseCH4 = ?perform_post("https://localhost/api/channels", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}], imersia_misc:safely_encode_json(AnotherChannel)),
    ?assert_status(200, ResponseCH4),
    % Grab the ChannelID
    {_, _, _, ChannelBodyJSON3} = ResponseCH4,
    ChannelID2 = ej:get({"channelid"}, jiffy:decode(ChannelBodyJSON3)),

    % Get a list of channels for the user identified by the Session
    ResponseCH5 = ?perform_get("https://localhost/api/channels", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}]),
    ?assert_status(200, ResponseCH5),
    {_, _, _, ChannelBodyJSON4} = ResponseCH5,
    ChannelID3 = ej:get({1, "channelid"}, jiffy:decode(ChannelBodyJSON4)),

    % Check that the ID matches at least one of those created
    if ((ChannelID3 == ChannelID1) or (ChannelID3 == ChannelID2)) -> ok; true -> throw("Incorrect Channel IDs") end,

    % ----------------------------------------------------------------------------------------------------
    % Test Geobots
    % ----------------------------------------------------------------------------------------------------
    GeobotName = <<"Roys_Geobot">>,
    GeobotName2 = <<"Roys_Other_Geobot">>,
    GeobotDescription = <<"This is my first Geobot">>,
    AnGeobot = imersia_misc:record_to_json(#geobot{
        name = GeobotName,
        description = GeobotDescription
    }, true),
    AnotherGeobot = imersia_misc:record_to_json(#geobot{
        name = GeobotName2,
        description = GeobotDescription
    }, true),
    _WrongGeobotID = imersia_db:new_id(),

    % Add a couple of Geobots
    ResponseGeobot1 = ?perform_post("https://localhost/api/geobots", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {channelid, ChannelID1}], imersia_misc:safely_encode_json(AnGeobot)),
    ?assert_status(200, ResponseGeobot1),
    {_, _, _, Geobot1} = ResponseGeobot1,
    GeobotID1 = ej:get({"geobotid"}, jiffy:decode(Geobot1)),

    ResponseGeobot2 = ?perform_post("https://localhost/api/geobots", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {channelid, ChannelID1}], imersia_misc:safely_encode_json(AnotherGeobot)),
    ?assert_status(200, ResponseGeobot2),
    {_, _, _, Geobot2} = ResponseGeobot2,
    GeobotID2 = ej:get({"geobotid"}, jiffy:decode(Geobot2)),

    % Get the list of Geobots
    ResponseGeobot3 = ?perform_get("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {channelid, ChannelID1}]),
    ?assert_status(200, ResponseGeobot3),

    % Check there are 2
    {_, _, _, GeobotList1} = ResponseGeobot3,
    GeobotListJSON = jiffy:decode(GeobotList1),
    2 = length(GeobotListJSON),

    % Get back a Geobot, and check it's details
    ResponseGeobot4 = ?perform_get("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}]),
    ?assert_status(200, ResponseGeobot4),
    {_, _, _, GeobotBack1} = ResponseGeobot4,
    GeobotName = ej:get({"name"}, jiffy:decode(GeobotBack1)),

    % Get back another Geobot, and check it's details
    ResponseGeobot5 = ?perform_get("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID2}]),
    ?assert_status(200, ResponseGeobot5),
    {_, _, _, GeobotBack2} = ResponseGeobot5,
    GeobotName2 = ej:get({"name"}, jiffy:decode(GeobotBack2)),

    ResponseGeobot6 = ?perform_delete("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID2}]),
    ?assert_status(200, ResponseGeobot6),

    % Try getting a Geobot that no longer exists
    ResponseGeobot7 = ?perform_get("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID2}]),
    ?assert_status(404, ResponseGeobot7),

    % Change the details of a Geobot
    ResponseGeobot8 = ?perform_put("https://localhost/api/geobots", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}], imersia_misc:safely_encode_json(AnotherGeobot)),
    ?assert_status(200, ResponseGeobot8),

    % Get back the changed Geobot, and check it's details
    ResponseGeobot9 = ?perform_get("https://localhost/api/geobots", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}]),
    ?assert_status(200, ResponseGeobot9),
    {_, _, _, GeobotBack3} = ResponseGeobot9,
    GeobotName2 = ej:get({"name"}, jiffy:decode(GeobotBack3)),

    % ----------------------------------------------------------------------------------------------------
    % Test Metadata
    % ----------------------------------------------------------------------------------------------------

    MetadataKey1 = <<"Fred">>,
    MetadataKey2 = <<"Peter">>,
    MetadataKey3 = <<"Alfred">>,
    Metadata1 = imersia_misc:record_to_json(#metadata{
            key = MetadataKey1,
            value = 23
        }, true),
    Metadata2 = imersia_misc:record_to_json(#metadata{
            key = MetadataKey2,
            value = <<"This is a string">>
        }, true),
    Metadata3 = imersia_misc:record_to_json(#metadata{
            key = MetadataKey3,
            value = <<"{hello:[1,2,\"three\"]}">>
        }, true),

    % Set a new metadata key/value
    ResponseMD1 = ?perform_post("https://localhost/api/metadata", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}], imersia_misc:safely_encode_json(Metadata1)),
    ?assert_status(200, ResponseMD1),

    % Get the metadata key/value
    ResponseMD2 = ?perform_get("https://localhost/api/metadata", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}, {key, binary_to_list(MetadataKey1)}]),
    ?assert_status(200, ResponseMD2),

    {_, _, _, MetadataResp2} = ResponseMD2,
    23 = ej:get({"value"}, jiffy:decode(MetadataResp2)),
    MetadataID1 = ej:get({"metadataid"}, jiffy:decode(MetadataResp2)),

    % Set the metadata using a key/value pair in the body data
    ResponseMD3 = ?perform_post("https://localhost/api/metadata", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}], imersia_misc:safely_encode_json(Metadata2)),
    ?assert_status(200, ResponseMD3),

    % Change the key and value for an existing metadata
    ResponseMD4 = ?perform_put("https://localhost/api/metadata", [?POST_TYPE, {developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}, {metadataid, MetadataID1}], imersia_misc:safely_encode_json(Metadata3)),
    ?assert_status(200, ResponseMD4),

    % Now the metadata should not be available by the old key
    ResponseMD5 = ?perform_get("https://localhost/api/metadata", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}, {key, binary_to_list(MetadataKey1)}]),
    ?assert_status(404, ResponseMD5),

    % Get a list of Metadata
    ResponseMD6 = ?perform_get("https://localhost/api/metadata", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}]),
    ?assert_status(200, ResponseMD6),

    % There should be two
    {_, _, _, MetadataResp6} = ResponseMD6,
    MetadataListJSON = jiffy:decode(MetadataResp6),
    2 = length(MetadataListJSON),

    % Get the changed Metadata with the new key
    ResponseMD7 = ?perform_get("https://localhost/api/metadata", [{developerid, DeveloperID}, {location, Location}, {sessionid, SessionID2}, {geobotid, GeobotID1}, {key, binary_to_list(MetadataKey3)}]),
    ?assert_status(200, ResponseMD7),

    % Should have the same value as before
    {_, _, _, MetadataResp7} = ResponseMD7,
    <<"{hello:[1,2,\"three\"]}">> = ej:get({"value"}, jiffy:decode(MetadataResp7)),

    % Delete a channel
    % ResponseCH6 = ?perform_delete("https://localhost/api/channels", [{sessionid, SessionID2}, {channelid, ChannelID1}]),
    % erlang:display(ResponseCH6),

    % Delete the test user
    ok = delete_user_if_exists(Connection, UserEmail),

    {ok, all_tests_passed}.


% Delete the test user ahead of time - in case there was one left over from a failed test earlier
delete_user_if_exists(Connection, UserEmail) ->
    case imersia_db:user_session_new(Connection, <<"123456ABCDEF">>, UserEmail) of
        {ok, SessionID} ->
            {ok, UserID} = imersia_db:user_id_from_sessionid(Connection, SessionID),
            imersia_db:user_delete(Connection, SessionID, UserID), ok;
        {error, useremail} -> ok
    end.

% ----------------------------------------------------------------------------------------------------
