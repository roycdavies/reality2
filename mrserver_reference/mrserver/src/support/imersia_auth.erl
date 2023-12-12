% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Various functions related to authentication
% ----------------------------------------------------------------------------------------------------

-module(imersia_auth).
-include("../imersia_datatypes.hrl").

-export([check_user/3, check_channel/5, check_geobot/6, check_channel_attribute/6, check_geobot_attribute/6]).



% ----------------------------------------------------------------------------------------------------
% Just convert the Method to an atom - the rest of the checking occurs elsewhere in the API call.
% ----------------------------------------------------------------------------------------------------
check_user(_, <<"HEAD">>, _) -> {ok, head};
check_user(_, <<"OPTIONS">>, _) -> {ok, options};
check_user(_, <<"GET">>, _) -> {ok, get};
check_user(_, <<"POST">>, _) -> {ok, post};
check_user(_, <<"PUT">>, _) -> {ok, put};
check_user(_, <<"DELETE">>, _) -> {ok, delete};
check_user(_, _, _) -> {error, sessionid}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the access rights to a channel
% Parameters:
% - Connection - DB Connection
% - Method - API method
% - ChannelID - The Channel's ID
% - UserID - the loggedin User's ID
% - OtherUserID - a User ID that we might be wanting to get channel details for
% - ContextIDs - An array of contextIDs that might alter the user's capabilities
% ----------------------------------------------------------------------------------------------------
% check_channel(_, Method, _, undefined, _) when Method /= <<"GET">> -> {error, sessionid};

% Check that a specified Channel exists
check_channel(_, <<"HEAD">>, _, undefined, _) -> {error, sessionid};
check_channel(_, <<"HEAD">>, undefined, _, _) -> {error, channelid};
check_channel(Connection, <<"HEAD">>, ChannelID, UserID, ContextIDs) ->
    check_channel_attribute(Connection, head, ChannelID, UserID, ContextIDs, <<"read">>);

% Anyone can read about the API
check_channel(_, <<"OPTIONS">>, _, _, _) -> {ok, options};

% Getting a list of channels
check_channel(_, <<"GET">>, undefined, _, _) -> {ok, channellist};

% Getting a single channel's details
check_channel(Connection, <<"GET">>, ChannelID, UserID, ContextIDs) ->
    check_channel_attribute(Connection, get, ChannelID, UserID, ContextIDs, <<"read">>);

% You can only create channels you will own
check_channel(_, <<"POST">>, _, undefined, _) -> {error, sessionid};
check_channel(_, <<"POST">>, _, _, _) -> {ok, post};

% Only the owner can edit their channels, no one else.
check_channel(_, <<"PUT">>, _, undefined, _) -> {error, sessionid};
check_channel(_, <<"PUT">>, undefined, _, _) -> {error, channelid};
check_channel(Connection, <<"PUT">>, ChannelID, UserID, _) ->
    case imersia_db:channel_getdetails(Connection, ChannelID) of
        {ok, Channel} ->
            case Channel#channel.ownerid of
                UserID -> {ok, put};
                _ -> {error, channelid}
            end;
        _ -> {error, channelid}
    end;

% You can only delete channels (or delete from) you own
check_channel(_, <<"DELETE">>, _, undefined, _) -> {error, sessionid};
check_channel(_, <<"DELETE">>, undefined, _, _) -> {error, channelid};
check_channel(Connection, <<"DELETE">>, ChannelID, UserID, _) ->
    case imersia_db:channel_getdetails(Connection, ChannelID) of
        {ok, Channel} ->
            case Channel#channel.ownerid of
                UserID -> {ok, delete};
                _ -> {error, channelid}
            end;
        _ -> {error, channelid}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the access rights to a Geobot
% ----------------------------------------------------------------------------------------------------
% check_geobot(_, _, _, _, undefined, _) -> {error, sessionid};

% Check that a specified Geobot exists and is allowed to be seen
check_geobot(_, <<"HEAD">>, undefined, _, _, _) -> {error, geobotid};
check_geobot(Connection, <<"HEAD">>, GeobotID, undefined, UserID, ContextIDs) ->
    case imersia_db:geobot_getchannelid(Connection, GeobotID) of
        {ok, ChannelID} ->
            check_geobot(Connection, <<"HEAD">>, GeobotID, ChannelID, UserID, ContextIDs);
        _ -> {error, geobotid}
    end;
check_geobot(Connection, <<"HEAD">>, GeobotID, ChannelID, UserID, ContextIDs) ->
    case check_channel_attribute(Connection, head, ChannelID, UserID, ContextIDs, <<"read">>) of
        {ok, _} ->
            check_geobot_attribute(Connection, head, GeobotID, UserID, ContextIDs, <<"read">>);
        {error, Reason} ->
            {error, Reason}
    end;

% Anyone can read about the API
check_geobot(_, <<"OPTIONS">>, _, _, _, _) -> {ok, options};

% Getting a list of geobots
check_geobot(_, <<"GET">>, undefined, undefined, _, _) -> {error, id};
check_geobot(Connection, <<"GET">>, undefined, ChannelID, UserID, ContextIDs) ->
    check_channel_attribute(Connection, geobotlist, ChannelID, UserID, ContextIDs, <<"list">>);

% Getting a single geobot's details
check_geobot(Connection, <<"GET">>, GeobotID, undefined, UserID, ContextIDs) ->
    case imersia_db:geobot_getchannelid(Connection, GeobotID) of
        {ok, ChannelID} ->
            check_geobot(Connection, <<"GET">>, GeobotID, ChannelID, UserID, ContextIDs);
        _ -> {error, geobotid}
    end;
check_geobot(Connection, <<"GET">>, GeobotID, ChannelID, UserID, ContextIDs) ->
    case check_channel_attribute(Connection, get, ChannelID, UserID, ContextIDs, <<"read">>) of
        {ok, _} ->
            check_geobot_attribute(Connection, get, GeobotID, UserID, ContextIDs, <<"read">>);
        {error, Reason} ->
            {error, Reason}
    end;

% Check if the user owns the channel in order to make changes to it when there is no Geobot specified
check_geobot(_, <<"POST">>, _, _, undefined, _) -> {error, sessionid};
check_geobot(_, <<"POST">>, _, undefined, _, _) -> {error, channelid};
check_geobot(Connection, <<"POST">>, _, ChannelID, UserID, ContextIDs) ->
    check_channel_attribute(Connection, post, ChannelID, UserID, ContextIDs, <<"add">>);

% Check the user can be allowed to edit the Geobot
check_geobot(_, <<"PUT">>, _, _, undefined, _) -> {error, sessionid};
check_geobot(_, <<"PUT">>, undefined, _, _, _) -> {error, geobotid};
check_geobot(Connection, <<"PUT">>, GeobotID, _, UserID, ContextIDs) ->
    check_geobot_attribute(Connection, put, GeobotID, UserID, ContextIDs, <<"edit">>);

check_geobot(_, <<"DELETE">>, _, _, undefined, _) -> {error, sessionid};
check_geobot(_, <<"DELETE">>, undefined, _, _, _) -> {error, geobotid};
check_geobot(Connection, <<"DELETE">>, GeobotID, _, UserID, ContextIDs) ->
    case imersia_db:geobot_getchannelid(Connection, GeobotID) of
        {ok, ChannelID} ->
            check_channel_attribute(Connection, delete, ChannelID, UserID, ContextIDs, <<"remove">>);
        _ -> {error, geobotid}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check for a specific attribute
% ----------------------------------------------------------------------------------------------------
check_channel_attribute(_, _, undefined, _, _, _) -> {error, channelid};
check_channel_attribute(Connection, put, ChannelID, UserID, ContextIDs, Attribute) ->
    check_channel_attribute(Connection, put, writing, ChannelID, UserID, ContextIDs, Attribute);
check_channel_attribute(Connection, post, ChannelID, UserID, ContextIDs, Attribute) ->
    check_channel_attribute(Connection, post, writing, ChannelID, UserID, ContextIDs, Attribute);
check_channel_attribute(Connection, delete, ChannelID, UserID, ContextIDs, Attribute) ->
    check_channel_attribute(Connection, delete, writing, ChannelID, UserID, ContextIDs, Attribute);
check_channel_attribute(Connection, Command, ChannelID, UserID, ContextIDs, Attribute) ->
    check_channel_attribute(Connection, Command, reading, ChannelID, UserID, ContextIDs, Attribute).

check_channel_attribute(Connection, Command, reading, ChannelID, UserID, ContextIDs, Attribute) ->
    case imersia_db:channel_getdetails(Connection, ChannelID) of
        {ok, Channel} ->
            case Channel#channel.ownerid of
                UserID -> {ok, Command}; % User owns the channel, so OK
                _ -> % User doesn't own the channel, so check Contexts if hidden
                    case Channel#channel.hidden of
                        true ->
                            case imersia_db:context_is_allowed(Connection, ChannelID, ContextIDs, Attribute) of
                                true -> {ok, Command};
                                false -> {error, context}
                            end;
                        false -> {ok, Command} % By default when reading, you are allowed
                    end
            end;
        _ -> {error, channelid}
    end;
check_channel_attribute(Connection, Command, writing, ChannelID, UserID, ContextIDs, Attribute) ->
    case imersia_db:channel_getdetails(Connection, ChannelID) of
        {ok, Channel} ->
            case Channel#channel.ownerid of
                UserID -> {ok, Command}; % User owns the channel, so OK
                _ -> % User doesn't own the channel, so check Contexts
                    case imersia_db:context_is_allowed(Connection, ChannelID, ContextIDs, Attribute) of
                        true -> {ok, Command};
                        false -> {error, context}
                    end
            end;
        _ -> {error, channelid}
    end.
% ----------------------------------------------------------------------------------------------------
check_geobot_attribute(_, _, undefined, _, _, _) -> {error, geobotid};
check_geobot_attribute(Connection, put, GeobotID, UserID, ContextIDs, Attribute) ->
    check_geobot_attribute(Connection, put, writing, GeobotID, UserID, ContextIDs, Attribute);
check_geobot_attribute(Connection, post, GeobotID, UserID, ContextIDs, Attribute) ->
    check_geobot_attribute(Connection, post, writing, GeobotID, UserID, ContextIDs, Attribute);
check_geobot_attribute(Connection, delete, GeobotID, UserID, ContextIDs, Attribute) ->
    check_geobot_attribute(Connection, delete, writing, GeobotID, UserID, ContextIDs, Attribute);
check_geobot_attribute(Connection, Command, GeobotID, UserID, ContextIDs, Attribute) ->
    check_geobot_attribute(Connection, Command, reading, GeobotID, UserID, ContextIDs, Attribute).

check_geobot_attribute(Connection, Command, reading, GeobotID, UserID, ContextIDs, Attribute) ->
    case imersia_db:geobot_getdetails(Connection, GeobotID) of
        {ok, Geobot} ->
            case Geobot#geobot.ownerid of
                UserID -> {ok, Command}; % User owns the channel, so OK
                _ -> % User doesn't own the channel, so check Contexts if hidden
                    case Geobot#geobot.hidden of
                        true ->
                            case imersia_db:context_is_allowed(Connection, GeobotID, ContextIDs, Attribute) of
                                true -> {ok, Command};
                                false -> {error, context}
                            end;
                        false -> {ok, Command} % By default when reading, you are allowed
                    end
            end;
        _ -> {error, geobotid}
    end;
check_geobot_attribute(Connection, Command, writing, GeobotID, UserID, ContextIDs, Attribute) ->
    case imersia_db:geobot_getdetails(Connection, GeobotID) of
        {ok, Geobot} ->
            case Geobot#geobot.ownerid of
                UserID -> {ok, Command}; % User owns the channel, so OK
                _ -> % User doesn't own the channel, so check Contexts if hidden
                    case imersia_db:context_is_allowed(Connection, GeobotID, ContextIDs, Attribute) of
                        true -> {ok, Command};
                        false -> {error, context}
                    end
            end;
        _ -> {error, geobotid}
    end.
% ----------------------------------------------------------------------------------------------------
