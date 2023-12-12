% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database abstract data structure functions that have to be implemented
% ----------------------------------------------------------------------------------------------------

-spec init() -> error | ok.
-spec connect() -> Connection :: port().
-spec drop() -> error | ok.

% ----------------------------------------------------------------------------------------------------
% User / Companion functions
% ----------------------------------------------------------------------------------------------------

% Get the UserID given the user's email address
-spec user_id(Connection :: port(), UserEmail :: binary()) ->
    {ok, uuid()} |              % Everything OK, user exists
    {error, useremail} |        % A user with this email address doesn't exist
    {error, database}.          % An error occured at the database

% Get the UserEmail given the user's ID
-spec user_email(Connection :: port(), UserID :: binary()) ->
    {ok, binary} |              % Everything OK, user exists
    {error, useremail} |        % A user with this ID doesn't exist
    {error, database}.          % An error occured at the database

% Get the UserID given the sessionID.  Doesn't gaurantee that the UserID is valid (ie user might have been deleted).
-spec user_id_from_sessionid(Connection :: port(), SessionID :: uuid()) ->
    {ok, uuid()} |              % Everything OK, the user with this
    {error, sessionid} |        % SessionID doesn't exists
    {error, database}.          % An error occured at the database

% Performs a log in for the given user, returning a session ID (or error)
-spec user_login(Connection :: port(), UserID :: uuid(), UserEmail :: binary(), Password :: binary()) ->
    {ok, logged_in} |           % All OK and Logged in (can now go and get a SessionID)
    {error, userid} |           % User with this ID doesn't exist
    {error, useremail} |        % User with the email address doesn't exist, or doesn't match userID
    {error, password} |         % Password for this user is incorrect
    {error, database}.          % An error occured at the database

% Logs out the current session ID (others for the same user are left unaffected)
-spec user_logout(Connection :: port(), SessionID :: uuid()) ->
    {ok, logged_out} |          % All OK and logged out (SessionID has been made invalid)
    {error, sessionid} |        % SessionID is invalid
    {error, database}.          % An error occured at the database

% Create (register) a new user
-spec user_new(Connection :: port(), UserEmail :: binary(), Password :: binary(), UserDetails :: #user_details{}, Location :: #location{}) ->
    {ok, uuid()} |              % All OK, User created.  New UserID returned
    {error, useremail} |        % A user with this email address already exists
    {error, database}.          % An error occured at the database

% Get a list of all the current users on this node.
-spec user_list(Connection :: port()) ->
    {ok, UserList :: list(#user{})} |   % All OK, a list of Users is returned (or empty list if there are none)
    {error, database}.          % An error occured at the database

% Delete an existing user, first checking the SessionID to be sure
-spec user_delete(Connection :: port(), SessionID :: uuid(), UserID :: uuid()) ->
    {ok, deleted} |             % All OK, User deleted
    {error, sessionid} |        % Invalid SessionID
    {error, userid} |           % UserID doesn't match user identified with SessionID
    {error, file} |             % Error deleting the files
    {error, database}.          % An error occured at the database

% Check that a SessionID exists and is for this user
-spec user_session_exists(Connection :: port(), SessionID :: uuid(), UserID :: uuid()) ->
    {ok, exists} |              % OK, SessionID exists
    {error, sessionid} |        % The SessionID doesn't exist
    {error, userid} |           % The SessionID isn't for the given UserID
    {error, database}.          % An error occured at the database

% Create a new session for the given User
-spec user_session_new(Connection :: port(), Token :: binary(), UserEmail :: binary()) ->
    {ok, SessionID :: uuid()} | % All OK, New Session created
    {error, useremail} |        % User with that email address doesn't exist
    {error, database}.          % An error occured at the database

% Create an aged session for the given User - this is to be able test the deletion of egeobotired sessions
-spec user_session_new(Connection :: port(), Token :: binary(), UserEmail :: binary(), Age :: float()) ->
    {ok, SessionID :: uuid()} | % All OK, New Session created
    {error, useremail} |        % User with that email address doesn't exist
    {error, database}.          % An error occured at the database

% Get the user's details (without the password) given the SessionID
-spec user_get_details(Connection :: port(), SessionID :: uuid()) ->
    {ok, UserDetails :: #user_details{}} |  % All OK, returns the User Details
    {error, sessionid} |        % Invalid SessionID
    {error, database}.          % An error occured at the database

% Get the user's details given the UserID
-spec user_get_details_by_userid(Connection :: port(), UserID :: uuid()) ->
    {ok, UserDetails :: #user_details{}} |  % All OK, returns the User Details
    {error, userid} |           % Invalid SessionID
    {error, database}.          % An error occured at the database

% Set or change the user's details (except password).  Any fields set to null are left unchanged.
-spec user_set_details(Connection :: port(), SessionID :: uuid(), UserDetails :: #user_details{}) ->
    {ok, updated} |             % All OK, User's details updated
    {error, sessionid} |        % Invalid SessionID
    {error, database}.          % An error occured at the database

% Change the user's password, first checking the old password and user's email address to be certain
-spec user_change_password(Connection :: port(), SessionID :: uuid(), NewPassword :: binary()) ->
    {ok, updated} |             % All OK, User's password updated
    {error, sessionid} |        % Invalid SessionID
    {error, database}.          % An error occured at the database

% Check the SessionID against one of UserID or UserEmail and return true if they match.  UserID has precedence.  False otherwise.
-spec user_exists_and_matches(Connection :: port(), SessionID :: uuid(), UserID :: uuid(), UserEmail :: binary()) ->
    {ok, matches} |             % All the parameters match and are consistent
    {error, sessionid} |        % Invalid SessionID
    {error, userid} |           % UserID doesn't match
    {error, useremail} |        % UserEmail doesn't match
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Channel Functions
% ----------------------------------------------------------------------------------------------------

% Check whether a channel exists
-spec channel_exists(Connection :: port(), ChannelID :: uuid()) ->
    {ok, exists} |              % All OK, channel exists
    {error, channelid} |        % Channel doesn't exist
    {error, database}.          % An error occured at the database

% Create a new Channel
-spec channel_new(Connection :: port(), UserID :: uuid(), Name :: binary(), Details :: #channel{}) ->
    {ok, ChannelID :: uuid()} | % All OK, new Channel created, ChannelID returned.
    {error, name} |             % A Channel with this name already exists
    {error, userid} |           % A user with this ID doesn't exist
    {error, database}.          % An error occured at the database

% Create a new Channel with a specific ID
-spec channel_new(Connection :: port(), UserID :: uuid(), ChannelID :: uuid(), Name :: binary(), Details :: #channel{}) ->
    {ok, ChannelID :: uuid()} | % All OK, new Channel created, ChannelID returned.
    {error, name} |             % A Channel with this name already exists
    {error, userid} |           % A user with this ID doesn't exist
    {error, database}.          % An error occured at the database

% Change the name of an existing channel
-spec channel_rename(Connection :: port(), ChannelID :: uuid(), Name :: binary()) ->
    {ok, updated} |             % All OK, name changed
    {error, name} |             % A Channel with this name already exists
    {error, channelid} |        % A Channel with this ChannelID doesn not exist
    {error, database}.          % An error occured at the database

% Get the ChannelID from the Channel name
-spec channel_id(Connection :: port(), Name :: binary()) ->
    {ok, ChannelID :: uuid()} | % All OK, ChannelID returned
    {error, name} |             % A Channel with this name does not exist
    {error, database}.          % An error occured at the database

% List the channels for the specified UserID (or if UserID is null, all channels on this node).
% If ShowHidden is true, then hidden channels are also shown, but only if the Channels are owned by VisitorID.
-spec channel_list(Connection :: port(), UserID :: uuid(), VisitorID :: uuid(), ShowHidden :: boolean(), ContextIDs :: list(binary())) ->
    {ok, ChannelList :: list(#channel{})} |   % All OK, a list of channels is returned (or empty list if there are none)
    {error, database}.          % An error occured at the database

-spec channel_list(Connection :: port(), UserID :: uuid(), VisitorID :: uuid(), ShowHidden :: boolean()) ->
    {ok, ChannelList :: list(#channel{})} |   % All OK, a list of channels is returned (or empty list if there are none)
    {error, database}.          % An error occured at the database

% List all the channels in this MR Server
-spec channel_list(Connection :: port()) ->
    {ok, ChannelList :: list(#channel{})} |   % All OK, a list of channels is returned (or empty list if there are none)
    {error, database}.          % An error occured at the database

% Get the details of the specified Channel.
-spec channel_getdetails(Connection :: port(), ChannelID :: uuid()) ->
    {ok, #channel{}} |          % All OK, returns the channel
    {error, channelid} |        % Channel with that ID does not exist
    {error, database}.          % An error occured at the database

% Get the details of the specified Channel, checking if the owner matches the visitor's useid.
-spec channel_getdetails(Connection :: port(), ChannelID :: uuid(), VisitorID :: uuid()) ->
    {ok, #channel{}} |          % All OK, returns the channel
    {error, channelid} |        % Channel with that ID does not exist or is hidden to this user
    {error, database}.          % An error occured at the database

% Set the details of the specified Channel.
-spec channel_setdetails(Connection :: port(), ChannelID :: uuid(), Details :: #channel{}) ->
    {ok, updated} |             % All OK, channel updated.
    {error, channelid} |        % Channel with that ID does not exist
    {error, database}.          % An error occured at the database

% Delete specified Channel
-spec channel_delete(Connection :: port(), ChannelID :: uuid()) ->
    {ok, deleted} |             % All OK, channel deleted.
    {error, channelid} |        % Channel with that ID does not exist
    {error, geobotid} |             % There was a problem with one of the Geobots.
    {error, file} |             % Error deleting the files
    {error, database}.          % An error occured at the database

% Delete all the user's channels (and Geobots)
-spec channel_delete_all(Connection :: port(), UserID :: uuid()) ->
    {ok, deleted} |             % All OK, channel deleted.
    {error, userid} |           % A user with this ID doesn't exist
    {error, file} |             % Error deleting the files
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Geobot functions
% ----------------------------------------------------------------------------------------------------

% Check with the specified Geobot exists
-spec geobot_exists(Connection :: port(), GeobotID :: uuid()) ->
    {ok, exists} |              % All OK, Geobot exists
    {error, geobotid} |         % Geobot doesn't exist
    {error, database}.          % An error occured at the database

% Get the ChannelID for a Geobot
-spec geobot_getchannelid(Connection :: port(), GeobotID :: uuid()) ->
    {ok, ChannelID :: uuid()} | % All OK, ChannelID returned
    {error, geobotid} |         % a Geobot with this ID doesn't exist
    {error, database}.          % An error occured at the database

% Create a new Geobot
-spec geobot_new(Connection :: port(), ChannelID :: uuid(), UserID :: uuid(), Details :: #geobot{}) ->
    {ok, GeobotID :: uuid()} |  % All OK, new Channel created, ChannelID returned.
    {error, channelid} |        % A channel with this ID doesn't exist
    {error, database}.          % An error occured at the database

% Create a new Geobot with a specific ID
-spec geobot_new(Connection :: port(), ChannelID :: uuid(), UserID :: uuid(), GeobotID :: uuid(), Details :: #geobot{}) ->
    {ok, GeobotID :: uuid()} |  % All OK, new Channel created, ChannelID returned.
    {error, channelid} |        % A channel with this ID doesn't exist
    {error, database}.          % An error occured at the database

% List the Geobots in a channel, taking into account whether hidden or not
-spec geobot_list(Connection :: port(), ChannelID :: uuid(), VisitorID :: uuid(), ShowHidden :: boolean()) ->
    {ok, GeobotList :: list(#geobot{})} |   % All OK, a list of Geobots is returned (or empty list if there are none)
    {error, channelid} |        % A channel with this ID doesn't exist
    {error, database}.          % An error occured at the database

% List all the Geobots in this MR Server
-spec geobot_list(Connection :: port()) ->
    {ok, GeobotList :: list(#geobot{})} |   % All OK, a list of Geobots is returned (or empty list if there are none)
    {error, database}.          % An error occured at the database

% List all the Geobots within radius distance of the given location within a given Channel.
% If ChannelID is undefined, lists all Geobots on this MRServer at this location in all Channels.
% Takes into account whether Geobots are hidden or not.
-spec geobot_list(Connection :: port(), ChannelID :: uuid(), VisitorID :: uuid(), Location :: #location{}, Radius :: integer(), ShowHidden :: boolean()) ->
    {ok, GeobotList :: list(#geobot{})} |   % All OK, a list of Geobots is returned (or empty list if there are none)
    {error, channelid} |        % A channel with this ID doesn't exist (if not undefined)
    {error, database}.          % An error occured at the database

% Get the details of a Geobot
-spec geobot_getdetails(Connection :: port(), GeobotID:: uuid()) ->
    {ok, #geobot{}} |           % All OK, returns the X{}
    {error, geobotid} |         % Geobot with that ID does not exist
    {error, database}.          % An error occured at the database

% Get the details of a Geobot for a specific visiting user
-spec geobot_getdetails(Connection :: port(), GeobotID:: uuid(), VisitorID :: uuid()) ->
    {ok, #geobot{}} |           % All OK, returns the X{}
    {error, geobotid} |         % Geobot with that ID does not exist
    {error, database}.          % An error occured at the database

% Set the details of a Geobot
-spec geobot_setdetails(Connection :: port(), GeobotID :: uuid(), Details :: #channel{}) ->
    {ok, updated} |             % All OK, Geobot updated.
    {error, geobotid} |         % Geobot with that ID does not exist
    {error, database}.          % An error occured at the database

% Delete a Geobot
-spec geobot_delete(Connection :: port(), ChannelID :: uuid(), GeobotID :: uuid()) ->
    {ok, deleted} |             % All OK, Geobot deleted.
    {error, geobotid} |         % Geobot with that ID does not exist
    {error, file} |             % Error deleting the files
    {error, database}.          % An error occured at the database

% Delete all the Geobots in a channel
-spec geobot_delete_all(Connection :: port(), ChannelID :: uuid()) ->
    {ok, deleted} |             % All OK, all Geobots in the channel deleted.
    {error, geobotid} |         % There was a problem with one of the Geobots.
    {error, channelid} |        % Channel with that ID does not exist.
    {error, file} |             % Error deleting the files
    {error, database}.          % An error occured at the database
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Metadata functions
% ----------------------------------------------------------------------------------------------------

% Set up the database table for the metadata
-spec metadata_init(Connection :: port(), ID :: uuid()) ->
    {ok, created} |             % OK, metadata table created
    {ok, exists} |              % OK, metadata table already exists
    {error, database}.          % An error occured at the database

% Check whether the current Metadata Exists
-spec metadata_exists(Connection :: port(), ID :: uuid(), MetadataID :: uuid()) ->
    {ok, exists} |              % OK, metadata Exists
    {error, database}.          % An error occured at the database

% Drop the Metadata Table - only when fully deleting the entity
-spec metadata_drop(Connection :: port(), ID :: uuid()) ->
    {ok, deleted} |             % OK, metadata table deleted
    {error, database}.          % An error occured at the database

% Get the MetadataID given a Key - returns the first ID found with the Key.
-spec metadata_id(Connection:: port(), ID :: uuid(), Key :: binary()) ->
    {ok, MetadataID :: uuid()} |
    {error, key} |              % Metadata with key doesn't exist
    {error, database}.          % An error occured at the database

% List all the metadata for a given entity as defined by the ID (User, Channel, Geobot)
-spec metadata_list(Connection :: port(), ID :: uuid()) ->
    {ok, MetadataList :: list(#metadata{})} | % All OK, returns a list of metadata (or empty list if there are none)
    {error, database}.          % An error occured at the database

% Get the value of the metadata for the given entity
-spec metadata_get(Connection :: port(), ID :: uuid(), Key :: binary()) ->
    {ok, #metadata{}} |
    {error, key} |              % Metadata with key doesn't exist
    {error, database}.          % An error occured at the database

% Set the metadata value by the key.  This creates a new key/value pair if the key doesn't already exist.
-spec metadata_set(Connection :: port(), ID :: uuid(), Key :: binary(), Value :: binary()) ->
    {ok, updated} |             % OK, metadata set / updated
    {error, key} |              % Metadata with key doesn't exist
    {error, database}.          % An error occured at the database

% Delete the metadata key/value
-spec metadata_delete(Connection :: port(), ID :: uuid(), Key :: binary()) ->
    {ok, deleted} |             % OK, metadata deleted
    {error, key} |              % Metadata with key doesn't exist
    {error, database}.          % An error occured at the database

% Get the value of the metadata for the given entity given the metadata's ID, rather than the key
-spec metadata_get_by_id(Connection :: port(), ID :: uuid(), MetadataID :: uuid()) ->
    {ok, #metadata{}} |
    {error, database}.          % An error occured at the database

% Set the metadata value given the MetadataID
-spec metadata_set_by_id(Connection :: port(), ID :: uuid(), MetadataID :: uuid(), Metadata :: #metadata{}) ->
    {ok, updated} |             % OK, metadata set / updated
    {error, database}.          % An error occured at the database

% Delete the metadata key/value
-spec metadata_delete_by_id(Connection :: port(), ID :: uuid(), MetadataID :: uuid()) ->
    {ok, deleted} |             % OK, metadata deleted
    {error, database}.          % An error occured at the database

-spec metadata_delete_all(Connection :: port(), ID :: uuid()) ->
    {ok, deleted} |             % OK, metadata deleted
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Automations functions
% ----------------------------------------------------------------------------------------------------

% Set up the database table for the automation
-spec automation_init(Connection :: port(), GeobotID :: uuid()) ->
    {ok, created} |             % OK, automation table created
    {ok, exists} |              % OK, automation table already exists
    {error, database}.          % An error occured at the database

% Check whether the current Automation Exists
-spec automation_exists(Connection :: port(), GeobotID :: uuid(), AutomationID :: uuid()) ->
    {ok, exists} |              % OK, automation exists
    {error, database}.          % An error occured at the database

% Drop the Automation Table - only when fully deleting the entity
-spec automation_drop(Connection :: port(), GeobotID :: uuid()) ->
    {ok, deleted} |             % OK, automation table created
    {error, database}.          % An error occured at the database

-spec automation_id(Connection:: port(), GeobotID :: uuid(), Name :: binary()) ->
    {ok, AutomationID :: uuid()} |
    {error, name} |             % Automation with name doesn't exist
    {error, database}.          % An error occured at the database

% List all the automations for a given entity as defined by the GeobotID
-spec automation_list(Connection :: port(), GeobotID :: uuid()) ->
    {ok, AutomationList :: list(#automation{})} | % All OK, returns a list of automations (or empty list if there are none)
    {error, database}.          % An error occured at the database

% Get the details of the automation for the given entity
-spec automation_getdetails(Connection :: port(), GeobotID :: uuid(), AutomationID :: uuid()) ->
    {ok, #automation{}} |       % OK, Automation returned
    {error, automatioid} |      % Automation with ID doesn't exist
    {error, database}.          % An error occured at the database

% Get a list of the unique commands on a Geobot
-spec automation_getcommands(Connection :: port(), GeobotID :: uuid()) ->
    {ok, CommandsList :: list(binary())} |      % OK, Commands returned
    {error,geobotid} |          % Automation with ID doesn't exist
    {error, database}.          % An error occured at the database

% Set the automation details.
-spec automation_new(Connection :: port(), GeobotID :: uuid(), Automation :: #automation{}) ->
    {ok, AutomationID :: uuid()} | % OK, new automation created
    {error, automation} |       % Automation details invalid
    {error, database}.          % An error occured at the database

% Set the automation details.
-spec automation_setdetails(Connection :: port(), GeobotID :: uuid(), AutomationID :: uuid(), Automation :: #automation{}) ->
    {ok, updated} |             % OK, automation set / updated
    {error, automationid} |     % Automation with ID doesn't exist
    {error, automation} |       % Automation details invalid
    {error, database}.          % An error occured at the database

% Delete the automation
-spec automation_delete(Connection :: port(), GeobotID :: uuid(), AutomationID :: uuid()) ->
    {ok, deleted} |             % OK, automation deleted
    {error, automationid} |     % Automation with ID doesn't exist
    {error, database}.          % An error occured at the database

-spec automation_delete_all(Connection :: port(), GeobotID :: uuid()) ->
    {ok, deleted} |             % OK, automation deleted
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Analytics functions
% ----------------------------------------------------------------------------------------------------

% Log an event, with optional parameters
-spec analytics_log(Connection :: port(), ID :: uuid(), Event :: binary(),
                    Location :: binary(), Date :: datetime(), Parameters :: tuple()) ->
    {ok, inserted} |            % OK, event logged
    {error, database}.          % An error occured at the database

% Query analytics for the given ID
-spec analytics_query(Connection :: port(), ID :: uuid(), Event :: binary(),
                      StartDate :: datetime(), EndDate :: datetime()) ->
    {ok, StatsList :: list(tuple())} |  % List of statistics
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Context functions
% ----------------------------------------------------------------------------------------------------

% Set up the database table for the context
-spec context_init(Connection :: port(), ID :: uuid()) ->
    {ok, created} |             % OK, context table created
    {ok, exists} |              % OK, context table already exists
    {error, database}.          % An error occured at the database

% Check whether the current context Exists
-spec context_exists(Connection :: port(), ID :: uuid(), ContextID :: uuid()) ->
    {ok, exists} |              % OK, context Exists
    {error, database}.          % An error occured at the database

% Check whether the Attribute is allowed in the given contexts
-spec context_is_allowed(Connection :: port(), ID :: uuid(), ContextIDs :: list(uuid()), Attribute :: binary()) ->
    true |                      % Attribute is allowed in the given contexts
    false.                      % Attribute is not allowed in the given contexts

% Drop the context Table - only when fully deleting the entity
-spec context_drop(Connection :: port(), ID :: uuid()) ->
    {ok, deleted} |             % OK, context table deleted
    {error, database}.          % An error occured at the database

% List all the contexts for a given entity as defined by the ID (User, Channel, Geobot)
-spec context_list(Connection :: port(), ID :: uuid()) ->
    {ok, ContextList :: list(#context{})} | % All OK, returns a list of contexts (or empty list if there are none)
    {error, database}.          % An error occured at the database

% Get the capabilities of the context for the given entity given the context's ID
-spec context_get(Connection :: port(), ID :: uuid(), ContextID :: uuid()) ->
    {ok, #context{}} |
    {error, database}.          % An error occured at the database

% Set the capabilities of the context given the ContextID
-spec context_set(Connection :: port(), ID :: uuid(), ContextID :: uuid(), Capabilities :: list(binary())) ->
    {ok, updated} |             % OK, context set / updated
    {error, database}.          % An error occured at the database

% Delete the context
-spec context_delete(Connection :: port(), ID :: uuid(), ContextID :: uuid()) ->
    {ok, deleted} |             % OK, context deleted
    {error, database}.          % An error occured at the database

% Delete all the contexts for an entity
-spec context_delete_all(Connection :: port(), ID :: uuid()) ->
    {ok, deleted} |             % OK, all contexts deleted
    {error, database}.          % An error occured at the database

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Passcode functions
% ----------------------------------------------------------------------------------------------------
% Set up the database table for the context
-spec passcode_new(Connection :: port(), UserEmail :: binary()) ->
    {ok, created} |             % OK, passcode creatd and sent
    {error, smtp} |             % SMTP settings not set up in mrserver toml file
    {error, database}.          % An error occured at the database

% Check whether the current passcode is balid or not
-spec passcode_check(Connection :: port(), UserEmail :: binary(), Passcode :: binary()) ->
    {ok, checked} |             % OK, passcode is correct and has been checked in time, and belongs to the email address
    {error, invalid} |          % Passcode does not match the one against the email address (or doesn't exist at all)
    {error, timeout} |          % It is too late, the passcode is no longer Invalid
    {error, numtries} |         % Too many attemmpts have been made (max 3)
    {error, used} |             % Passcode has already been validated
    {error, database}.          % An error occured at the database
