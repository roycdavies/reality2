% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of channel functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_channel).

-include("../imersia_datatypes.hrl").

-export([
    channel_new/4, channel_new/5, channel_rename/3, channel_id/2, channel_exists/2, channel_list/4, channel_list/5, channel_list/1, channel_getdetails/2, channel_getdetails/3, channel_setdetails/3, channel_delete/2, channel_delete_all/2
]).


% ----------------------------------------------------------------------------------------------------
% Create a new channel
% ----------------------------------------------------------------------------------------------------
channel_new(Connection, UserID, Name, Details) ->
    ChannelID = imersia_db:new_id(),
    channel_new(Connection, UserID, ChannelID, Name, Details).

channel_new(_Connection, UserID, ChannelID, Name, Details) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"channels">>, #{<<"name">> => Name}) of
                undefined ->
                    % Name doesn't exist so carry on
                    DBEntry = #channel {
                        channelid = ChannelID,
                        ownerid = UserID,
                        name = Name,
                        description = Details#channel.description,
                        class = imersia_misc:convert_class(Details#channel.class),
                        imageurl = Details#channel.imageurl,
                        hidden = imersia_misc:convert_bool(Details#channel.hidden, false),
                        created = iso8601:format(calendar:universal_time()),
                        modified = iso8601:format(calendar:universal_time())
                    },
                    DBEntryMap = maps:remove(<<"channelid">>, imersia_misc:safely_convert_to_map(imersia_misc:record_to_json(DBEntry, false))),

                    % Create new database for this Channel and set the details
                    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
                        {ok, Connection3} ->
                            mc_worker_api:insert(Connection3, <<"details">>, DBEntryMap#{<<"_id">> => ChannelID}),
                            mc_worker_api:disconnect(Connection3),

                            % Insert entry into the admin database
                            AdminDBEntry = #{
                                <<"_id">> => ChannelID,
                                <<"ownerid">> => UserID,
                                <<"name">> => Name
                            },
                            mc_worker_api:insert(Connection2, <<"channels">>, AdminDBEntry),
                            imersia_db:metadata_init(Connection3, ChannelID),
                            imersia_db:context_init(Connection3, ChannelID),
                            mc_worker_api:disconnect(Connection2),
                            {ok, ChannelID};

                        _ ->
                            mc_worker_api:disconnect(Connection2),
                            {error, database}
                    end;
                _ ->
                    % Name already exists, so return error
                    mc_worker_api:disconnect(Connection2),
                    {error, name}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Rename the Channel
% ----------------------------------------------------------------------------------------------------
channel_rename(Connection, ChannelID, Name) ->
    % Check that a channel with this name doesn't already exist
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"channels">>, #{<<"name">> => Name}) of
                undefined ->
                    % If not, rename this channel to this new name
                    mc_worker_api:disconnect(Connection2),
                    do_channel_rename(Connection, ChannelID, Name);
                ExistingChannel ->
                    mc_worker_api:disconnect(Connection2),
                    % Also return ok if the name and channelid match (ie no renaming required)
                    ExistingID = maps:get(<<"_id">>, ExistingChannel),
                    if
                        ExistingID =/= ChannelID -> {error, name};
                        true -> {ok, updated}
                    end
            end;
        _ -> {error, database}
    end.

do_channel_rename(_Connection, ChannelID, Name) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection1} ->
            case mc_worker_api:find_one(Connection1, <<"channels">>, #{<<"_id">> => ChannelID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection1),
                    {error, channelid};
                _ ->
                    % Update database entry for this ChannelID
                    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
                        {ok, Connection2} ->
                            ChannelDetails = #{
                                <<"modified">> => iso8601:format(calendar:universal_time()),
                                <<"name">> => Name
                            },
                            Command = #{<<"$set">> => ChannelDetails},
                            mc_worker_api:update(Connection2, <<"details">>, #{<<"_id">> => ChannelID}, Command),
                            mc_worker_api:disconnect(Connection2),

                            % Update admin database
                            mc_worker_api:update(Connection1, <<"channels">>, #{<<"_id">> => ChannelID}, Command),
                            mc_worker_api:disconnect(Connection1),
                            {ok, updated};
                        _ ->
                            mc_worker_api:disconnect(Connection1),
                            {error, database}
                    end
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the Channel ID given the Channel Name
% ----------------------------------------------------------------------------------------------------
channel_id(_Connection, undefined) -> {error, name};
channel_id(_Connection, Name) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"channels">>, #{<<"name">> => Name}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, name};
                Details ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, maps:get(<<"_id">>, Details)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this channel exists
% ----------------------------------------------------------------------------------------------------
channel_exists(_Connection, undefined) -> {error, channelid};
channel_exists(_Connection, ChannelID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"channels">>, #{<<"_id">> => ChannelID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, channelid};
                _Details ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, exists}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the channels for this User taking into account if they are hidden or not
% ----------------------------------------------------------------------------------------------------
channel_list(Connection, UserID, VisitorID, ShowHidden) ->
    channel_list(Connection, UserID, VisitorID, ShowHidden, []).

channel_list(Connection, undefined, undefined, ShowHidden, ContextIDs) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"channels">>, {}) of
                {ok, Cursor} ->
                    ChannelIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    {ok, process_channel_list(Connection, ChannelIDList, undefined, undefined, ShowHidden, ContextIDs)};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, []}
            end;
        _ -> {error, database}
    end;

channel_list(Connection, UserID, VisitorID, ShowHidden, ContextIDs) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"channels">>, #{<<"ownerid">> => UserID}) of
                {ok, Cursor} ->
                    ChannelIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    {ok, process_channel_list(Connection, ChannelIDList, UserID, VisitorID, ShowHidden, ContextIDs)};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

% Get the channels for the Velocity Engine
channel_list(_Connection) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"channels">>, {}) of
                {ok, Cursor} ->
                    ChannelIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    {ok, ChannelIDList};
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {ChannelMap}) ->
    [maps:get(<<"_id">>, ChannelMap) | get_at_cursor(Cursor, mc_cursor:next(Cursor))].

process_channel_list(_, [], _, _, _, _) -> [];
process_channel_list(Connection, [ChannelID | ChannelIDs], UserID, VisitorID, ShowHidden, ContextIDs) ->
    case (channel_getdetails(dummy_connection, ChannelID)) of
        {ok, ChannelRecord} ->
            IsAllowed = imersia_db:context_is_allowed(Connection, ChannelRecord#channel.channelid, ContextIDs, <<"read">>),
            if
                ((ShowHidden and ChannelRecord#channel.hidden and (VisitorID == ChannelRecord#channel.ownerid)) or
                (not ChannelRecord#channel.hidden) or
                IsAllowed) ->
                    [ChannelRecord | process_channel_list(Connection, ChannelIDs, UserID, VisitorID, ShowHidden, ContextIDs)];
                true ->
                    process_channel_list(Connection, ChannelIDs, UserID, VisitorID, ShowHidden, ContextIDs)
            end;
        _ -> process_channel_list(Connection, ChannelIDs, UserID, VisitorID, ShowHidden, ContextIDs)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the details of the given channel
% ----------------------------------------------------------------------------------------------------
channel_getdetails(_Connection, ChannelID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"details">>, #{<<"_id">> => ChannelID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, channelid};
                ChannelRecord ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, channel_convert_to_record(ChannelRecord)}
            end;
        _ -> {error, database}
    end.

channel_getdetails(_Connection, ChannelID, VisitorID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find_one(Connection2, <<"details">>, #{<<"_id">> => ChannelID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection2),
                    {error, channelid};
                Details ->
                    ChannelRecord = channel_convert_to_record(Details),
                    mc_worker_api:disconnect(Connection2),
                    if
                        ((VisitorID == ChannelRecord#channel.ownerid) or (not ChannelRecord#channel.hidden)) ->
                            {ok, ChannelRecord};
                        true ->
                            {error, channelid}
                    end
            end;
        _ -> {error, database}
    end.

channel_convert_to_record(DetailsMap) ->
    #channel{
        channelid = maps:get(<<"_id">>, DetailsMap),
        ownerid = maps:get(<<"ownerid">>, DetailsMap),
        class = maps:get(<<"class">>, DetailsMap),
        name = maps:get(<<"name">>, DetailsMap),
        description = maps:get(<<"description">>, DetailsMap),
        imageurl = maps:get(<<"imageurl">>, DetailsMap),
        hidden = maps:get(<<"hidden">>, DetailsMap),
        created = iso8601:parse(maps:get(<<"created">>, DetailsMap)),
        modified = iso8601:parse(maps:get(<<"modified">>, DetailsMap))
    }.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the details for the given channel (except the name)
% ----------------------------------------------------------------------------------------------------
channel_setdetails(_Connection, ChannelID, Details) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection1} ->
            case mc_worker_api:find_one(Connection1, <<"channels">>, #{<<"_id">> => ChannelID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection1),
                    {error, channelid};
                _ ->
                    mc_worker_api:disconnect(Connection1),
                    case mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]) of
                        {ok, Connection2} ->
                            CheckNull = fun(_Key, Value) -> (Value /= null) and (Value /= undefined) end,
                            ChannelDetails = maps:filter(CheckNull, #{
                                <<"modified">> => iso8601:format(calendar:universal_time()),
                                <<"class">> => Details#channel.class,
                                <<"description">> => Details#channel.description,
                                <<"imageurl">> => Details#channel.imageurl,
                                <<"hidden">> => imersia_misc:convert_bool(Details#channel.hidden, null)
                            }),
                            Command = #{<<"$set">> => ChannelDetails},
                            mc_worker_api:update(Connection2, <<"details">>, #{<<"_id">> => ChannelID}, Command),
                            mc_worker_api:disconnect(Connection2),
                            {ok, updated};
                        _ -> {error, database}
                    end
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Table entry from the channels table, and the table of Geobots
% ----------------------------------------------------------------------------------------------------
channel_delete(Connection, ChannelID) ->
    case imersia_db:geobot_delete_all(Connection, ChannelID) of
        {ok, deleted} ->
            {ok, Connection2} = mc_worker_api:connect ([{database, <<"imersia_admin">>}]),
            mc_worker_api:delete(Connection2, <<"channels">>, #{<<"_id">> => ChannelID}),
            mc_worker_api:disconnect(Connection2),

            % Drop the channel's database
            {ok, Connection3} = mc_worker_api:connect ([{database, <<"imersia_", ChannelID/binary>>}]),
            mc_worker_api:command(Connection3, #{<<"dropDatabase">> => 1}),
            mc_worker_api:disconnect(Connection3),
            {ok, deleted};
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Channels for this user
% ----------------------------------------------------------------------------------------------------
channel_delete_all(Connection, UserID) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection2} ->
            case mc_worker_api:find(Connection2, <<"channels">>, #{<<"ownerid">> => UserID}) of
                {ok, Cursor} ->
                    ChannelIDList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection2),
                    process_channel_delete(Connection, ChannelIDList, UserID);
                _ ->
                    mc_worker_api:disconnect(Connection2),
                    {ok, deleted}
            end;
        _ -> {error, database}
    end.

process_channel_delete(_, [], _) -> {ok, deleted};
process_channel_delete(Connection, [ChannelID | ChannelIDs], UserID) ->
    case imersia_db:channel_delete(Connection, ChannelID) of
        {ok, deleted} -> process_channel_delete(Connection, ChannelIDs, UserID);
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
