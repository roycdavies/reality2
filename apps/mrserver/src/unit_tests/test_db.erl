% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Unit tests for the Database.  These should work regardless of the actual DB implementation chosen.
% ----------------------------------------------------------------------------------------------------
-module(test_db).

-include("../imersia_datatypes.hrl").

-export([test/0, test1/0]).


test() ->
    % Setup
    Connection = imersia_db:connect(),

    UserEmail = <<"fred@dagg.co.nz">>,
    Details = #user_details{
        firstname = <<"Fred">>,
        surname = <<"Dagg">>,
        nickname = <<"A Good Keen Man">>,
        imageurl = <<>>
    },
    Location = imersia_misc:unify_location(
        #location{
            latitude = 38.897,
            longitude = -77.036,
            altitude = 10
        }
    ),
    Details2 = #user_details{
        firstname = <<"John">>,
        % surname = null,
        nickname = <<"Another Good Keen Man">>
    },
    UserPass = <<"Password">>,
    WrongEmail = <<"bruce@balis.co.nz">>,
    WrongPass = <<"AnotherPassword">>,
    WrongID = imersia_db:new_id(),
    Token = <<"123456ABCDEF">>,

    ok = delete_user_if_exists(Connection, UserEmail),

    RoyDetails = #user_details{
        firstname = <<"Roy">>,
        surname = <<"Davies">>,
        nickname = <<"VRGuy">>,
        imageurl = <<>>
    },
    imersia_db:user_new(Connection, <<"roy@imersia.com">>, <<"1gelk0tt">>, RoyDetails, Location),

    erlang:display("*** Testing User Functions ***"),
    erlang:display(file:get_cwd()),
    % Tests
    % Create a new user
    {ok, UserID} = imersia_db:user_new(Connection, UserEmail, UserPass, Details, Location),
    % Creating a user with the same email address should fail
    {error, useremail} = imersia_db:user_new(Connection, UserEmail, UserPass, Details, Location),
    % Get the UserID of the new user
    {ok, UserID} = imersia_db:user_id(Connection, UserEmail),
    % Try getting a userID from a non-existent user
    {error, useremail} = imersia_db:user_id(Connection, WrongEmail),
    % Log in with the correct details
    {ok, logged_in} = imersia_db:user_login(Connection, UserID, UserEmail, UserPass),
    % Get a new SessionID
    {ok, SessionID} = imersia_db:user_session_new(Connection, Token, UserEmail),
    % Log out with a valid SessionID
    {ok, logged_out} = imersia_db:user_logout(Connection, SessionID),
    % Try logging in with a non-longer-valid SessionID
    {error, sessionid} = imersia_db:user_id_from_sessionid(Connection, SessionID),
    % Log in again
    {ok, logged_in} = imersia_db:user_login(Connection, UserID, UserEmail, UserPass),
    % Get a new SessionID
    {ok, SessionID2} = imersia_db:user_session_new(Connection, Token, UserEmail),
    % Test the user exists and matches function
    {ok, matches} = imersia_db:user_exists_and_matches(Connection, SessionID2, UserID, UserEmail),
    {error, sessionid} = imersia_db:user_exists_and_matches(Connection, undefined, UserID, UserEmail),
    {ok, matches} = imersia_db:user_exists_and_matches(Connection, SessionID2, undefined, UserEmail),
    {error, userid} = imersia_db:user_exists_and_matches(Connection, SessionID2, undefined, undefined),
    {ok, matches} = imersia_db:user_exists_and_matches(Connection, SessionID2, UserID, undefined),
    {error, userid} = imersia_db:user_exists_and_matches(Connection, SessionID2, WrongID, undefined),
    {error, useremail} = imersia_db:user_exists_and_matches(Connection, SessionID2, undefined, WrongEmail),
    {error, useremail} = imersia_db:user_exists_and_matches(Connection, SessionID2, undefined, <<"roy.c.davies@ieee.org">>),
    % Get the User Details
    {ok, UserDetails} = imersia_db:user_get_details(Connection, SessionID2),
    % Check these are the same as put into the DB
    #user{
        userid = UserID,
        useremail = UserEmail,
        details = Details,
        location = Location
    } = UserDetails,

    % Update a user's details.
    {ok, updated} = imersia_db:user_set_details(Connection, SessionID2, Details2),
    {ok, UserDetails2} = imersia_db:user_get_details(Connection, SessionID2),
    % Check these are the same as put into the DB, and that details not changed remain the same
    #user{
        userid = UserID,
        useremail = UserEmail,
        details = #user_details{
            firstname = <<"John">>,
            surname = <<"Dagg">>,
            nickname = <<"Another Good Keen Man">>,
            imageurl = <<>>
        },
        location = Location
    } = UserDetails2,

    % Check the error returned for an incorrect SessionID
    {error, sessionid} = imersia_db:user_set_details(Connection, Token, Details2),

    % Check error from giving an incorrect SessionID
    {error, sessionid} = imersia_db:user_id_from_sessionid(Connection, WrongID),
    % Check with wrong email address
    {error, useremail} = imersia_db:user_login(Connection, UserID, WrongEmail, UserPass),
    % Check with unmatched userID and email
    {error, userid} = imersia_db:user_login(Connection, WrongID, UserEmail, UserPass),
    % Check with wrong password
    {error, password} = imersia_db:user_login(Connection, UserID, UserEmail, WrongPass),
    % Change the password
    {ok, updated} = imersia_db:user_change_password(Connection, SessionID2, WrongPass),
    % Create an aged Session for testing purpose
    {ok, OldSessionID} = imersia_db:user_session_new(Connection, Token, UserEmail, ?MAX_SESSION_TIME + 2),
    % Relogin because all sessions deleted after password change
    {ok, SessionID3} = imersia_db:user_session_new(Connection, Token, UserEmail),
    % Old SessionID should no longer be valid
    {error, sessionid} = imersia_db:user_get_details(Connection, OldSessionID),
    % Check with new password (after having changed it above)
    {ok, logged_in} = imersia_db:user_login(Connection, UserID, UserEmail, WrongPass),
    % Check trying to delete a user given an invalid sessionid
    {error, sessionid} = imersia_db:user_delete(Connection, SessionID, UserID),
    % Check trying to delete a user given the wrong userID
    {error, userid} = imersia_db:user_delete(Connection, SessionID3, WrongID),
    % Check getting sessionID with wrong email address (ie non-existent user)
    {error, useremail} = imersia_db:user_session_new(Connection, Token, WrongEmail),

    % ----------------------------------------------------------------------------------------------------
    % Channel tests
    % ----------------------------------------------------------------------------------------------------

    ChannelName = <<"Freds Channel">>,
    AnotherChannelName = <<"Petes Channel">>,
    YetAnotherChannelName = <<"Alfreds Channel">>,
    AChannel = #channel{
        description = <<"A new Channel">>,
        imageurl = <<"http://www.imersia.com">>
    },
    WrongChannelID = imersia_db:new_id(),
    HiddenChannel = #channel{
        hidden = true
    },
    OtherID = imersia_db:new_id(),

    erlang:display("*** Testing Channel Functions ***"),

    % Create a new Channel
    {ok, ChannelID1} = imersia_db:channel_new(Connection, UserID, ChannelName, AChannel),

    % Check the channel now exists
    {ok, exists} = imersia_db:channel_exists(Connection, ChannelID1),

    % Create another new Channel
    {ok, ChannelID2} = imersia_db:channel_new(Connection, UserID, YetAnotherChannelName, AChannel),

    % Check the channel now exists
    {ok, exists} = imersia_db:channel_exists(Connection, ChannelID2),

    % Make sure we get the right ChannelID back
    {ok, ChannelID2} = imersia_db:channel_id(Connection, YetAnotherChannelName),

    % When the Channel with that name doesn't exist
    {error, name} = imersia_db:channel_id(Connection, AnotherChannelName),

    % Try creating a channel with the same name
    {error, name} = imersia_db:channel_new(Connection, UserID, ChannelName, AChannel),

    % Try renaming a channel with the wrong ID
    {error, channelid} = imersia_db:channel_rename(Connection, WrongChannelID, AnotherChannelName),

    % Rename the Channel
    {ok, updated} = imersia_db:channel_rename(Connection, ChannelID1, AnotherChannelName),

    % Try renaming the channel to one that already exists
    {error, name} = imersia_db:channel_rename(Connection, ChannelID1, YetAnotherChannelName),

    % List all the channels for the given user
    {ok, ChannelList} = imersia_db:channel_list(Connection, UserID, OtherID, true),
    2 = length(ChannelList),

    % Hide a channel
    {ok, updated} = imersia_db:channel_setdetails(Connection, ChannelID1, HiddenChannel),

    % List all the channels for the given user, by the user
    {ok, ChannelList2} = imersia_db:channel_list(Connection, UserID, UserID, true),
    2 = length(ChannelList2),

    % List all the non-hidden channels for the given user, by the user
    {ok, ChannelList3} = imersia_db:channel_list(Connection, UserID, UserID, false),
    1 = length(ChannelList3),

    % List all the channels for the given user, by another user
    {ok, ChannelList4} = imersia_db:channel_list(Connection, UserID, OtherID, true),
    1 = length(ChannelList4),

    % ----------------------------------------------------------------------------------------------------
    % Geobot tests
    % ----------------------------------------------------------------------------------------------------
    GeobotName = <<"A Geobot">>,
    GeobotName2 = <<"Another Geobot">>,
    AGeobot = #geobot{
        name = GeobotName,
        description = <<"This is a Geobot">>,
        location = #location{
            latitude = 36.897,
            longitude = -78.036,
            altitude = 0
        },
        imageurl = <<"http://animage.com/anotherimage.jpg">>
    },
    AnotherGeobot = #geobot{
        name = GeobotName2,
        description = <<"This is another Geobot">>,
        location = #location{
            latitude = 38.897,
            longitude = -77.036,
            altitude = 10
        },
        imageurl = <<"http://animage.com/animage.jpg">>
    },

    erlang:display("*** Testing Geobot Functions ***"),

    % Create a new Geobot
    {ok, GeobotID} = imersia_db:geobot_new(Connection, ChannelID1, UserID, AGeobot),

    % Check that it now exists and looks the same coming out as going in
    {ok, exists} = imersia_db:geobot_exists(Connection, GeobotID),
    {ok, GeobotDetails} = imersia_db:geobot_getdetails(Connection, GeobotID),
    GeobotName = GeobotDetails#geobot.name,
    %
    % % Check the number of Geobots now in the Channel
    % {ok, GeobotList} = imersia_db:geobot_list(Connection, ChannelID1, UserID, true),
    % 1 = length(GeobotList),

    % Create another Geobot and check it was created correctly
    {ok, GeobotID2} = imersia_db:geobot_new(Connection, ChannelID1, UserID, AnotherGeobot),
    {ok, GeobotDetails2} = imersia_db:geobot_getdetails(Connection, GeobotID2),
    GeobotName2 = GeobotDetails2#geobot.name,

    {ok, updated} = imersia_db:geobot_setdetails(Connection, GeobotID2, #geobot{name=GeobotName}),
    {ok, GeobotDetails3} = imersia_db:geobot_getdetails(Connection, GeobotID2),
    GeobotName = GeobotDetails3#geobot.name,
    <<"This is another Geobot">> = GeobotDetails3#geobot.description,

    % Make sure the number of Geobots is now 2
    {ok, GeobotList2} = imersia_db:geobot_list(Connection, ChannelID1, UserID, true),
    2 = length(GeobotList2),

    % Delete a Geobot, and check there is now only 1 left.
    {ok, deleted} = imersia_db:geobot_delete(Connection, ChannelID1, GeobotID2),
    {ok, GeobotList3} = imersia_db:geobot_list(Connection, ChannelID1, UserID, true),
    1 = length(GeobotList3),

    % Delete all the rest of the Geobots
    {ok, deleted} = imersia_db:geobot_delete_all(Connection, ChannelID1),

    % There should now be none left
    {ok, GeobotList4} = imersia_db:geobot_list(Connection, ChannelID1, UserID, true),
    0 = length(GeobotList4),

    % ----------------------------------------------------------------------------------------------------
    % Test some Metadata
    % ----------------------------------------------------------------------------------------------------
    erlang:display("*** Testing Metadata Functions ***"),

    {ok, MetadataID} = imersia_db:metadata_set(Connection, UserID, <<"Fred">>, <<"{foo:[\"bing\",2.3,true]}">>),

    {ok, Metadata} = imersia_db:metadata_get(Connection, UserID, <<"Fred">>),

    MetadataID = Metadata#metadata.metadataid,
    {ok, Metadata} = imersia_db:metadata_get_by_id(Connection, UserID, MetadataID),

    {ok, MetadataID2} = imersia_db:metadata_set(Connection, UserID, <<"Peter">>,  <<"[\"bing\", 2.3, true]">>), %<<"{foo:[bing,2.3,true]}">>),

    {ok, Metadata2} = imersia_db:metadata_get(Connection, UserID, <<"Peter">>),
    MetadataID2 = Metadata2#metadata.metadataid,
    erlang:display(Metadata2),

    {ok, MetadataList} = imersia_db:metadata_list(Connection, UserID),
    2 = length(MetadataList),

    {ok, deleted} = imersia_db:metadata_delete(Connection, UserID, <<"Fred">>),
    {ok, MetadataList2} = imersia_db:metadata_list(Connection, UserID),
    1 = length(MetadataList2),

    {ok, updated} = imersia_db:metadata_set_by_id(Connection, UserID, MetadataID2, #metadata{key = <<"Arthur">>}),
    {ok, Metadata3} = imersia_db:metadata_get_by_id(Connection, UserID, MetadataID2),
    <<"Arthur">> = Metadata3#metadata.key,
    {ok, updated} = imersia_db:metadata_set_by_id(Connection, UserID, MetadataID2, #metadata{value = <<"A String">>}),
    {ok, Metadata4} = imersia_db:metadata_get_by_id(Connection, UserID, MetadataID2),
    <<"A String">> = Metadata4#metadata.value,

    {ok, deleted} = imersia_db:metadata_delete_all(Connection, UserID),
    {ok, MetadataList3} = imersia_db:metadata_list(Connection, UserID),
    0 = length(MetadataList3),


    % ----------------------------------------------------------------------------------------------------
    % Test some Automations
    % ----------------------------------------------------------------------------------------------------
    erlang:display("*** Testing Automations ***"),

    Action1Text = <<"{\"command\":\"set\",\"parameters\":{\"key\":\"test\",\"value\":1}}">>,
    Action2Text = <<"{\"command\":\"send\",\"parameters\":{\"geobotid\":\"fred\",\"event\":\"test\",\"delay\":23,\"parameters\":[]}}">>,
    Transition1Text = <<"{\"state\":\"on\",\"event\":\"turnoff\",\"newstate\":\"off\",\"actions\":[", Action1Text/binary, ",", Action2Text/binary, "]}">>,
    Transition1JSON = imersia_misc:safely_decode_json(Transition1Text),
    Transition1 = imersia_misc:json_to_record(transition, Transition1JSON),

    Transition2Text = <<"{\"state\":\"off\",\"event\":\"turnon\",\"newstate\":\"on\",\"actions\":[", Action2Text/binary, ",", Action1Text/binary, "]}">>,
    Transition2JSON = imersia_misc:safely_decode_json(Transition2Text),
    Transition2 = imersia_misc:json_to_record(transition, Transition2JSON),

    Automation = #automation{
        name = <<"Test Automation">>,
        description = <<"A Description">>,
        commands = [<<"turnon">>],
        transitions = [Transition1, Transition2]
    },

    {ok, GeobotIDForAutomations} = imersia_db:geobot_new(Connection, ChannelID1, UserID, AGeobot),

    {ok, _AutomationID} = imersia_db:automation_new(Connection, GeobotIDForAutomations, Automation),

    {ok, AutomationList} = imersia_db:automation_list(Connection, GeobotIDForAutomations),
    1 = length(AutomationList),



    % ----------------------------------------------------------------------------------------------------
    % Clean up some channels
    % ----------------------------------------------------------------------------------------------------

    % Delete one channel
    {ok, deleted} = imersia_db:channel_delete(Connection, ChannelID1),

    % Check the channel now doesn't exist
    {error, channelid} = imersia_db:channel_exists(Connection, ChannelID1),

    % Delete the other channel
    {ok, deleted} = imersia_db:channel_delete(Connection, ChannelID2),

    % There should be no channels left
    {ok, ChannelList5} = imersia_db:channel_list(Connection, UserID, OtherID, true),
    0 = length(ChannelList5),

    % Channel should now be invalid
    {error, _} = imersia_db:geobot_list(Connection, ChannelID1, UserID, true),


    % ----------------------------------------------------------------------------------------------------
    % Final clean up of test user
    % ----------------------------------------------------------------------------------------------------

    % Try deleting a user
    {ok, deleted} = imersia_db:user_delete(Connection, SessionID3, UserID),
    % Try creating a user session for a user that has now been deleted
    {error, useremail} = imersia_db:user_session_new(Connection, Token, UserEmail),
    % Try getting testing for a user that no longer exists
    {error, sessionid} = imersia_db:user_exists_and_matches(Connection, SessionID3, UserID, undefined),

    {ok, all_tests_passed}.


test1() ->
    Connection = imersia_db:connect(),
    Result = imersia_db:geobot_list(Connection, undefined, undefined, {location, -36.867372, 174.755127, 0, null}, 100, true),
    erlang:display(Result),

    Result2 = imersia_db:geobot_list(Connection, undefined, undefined, {location, -36.867372, 174.755127, 0, null}, 10000, true),
    erlang:display(Result2),
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
