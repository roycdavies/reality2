% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of channel functions using MNesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_channel).

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

channel_new(Connection, UserID, ChannelID, Name, Details) ->
    % Check that name is unique
    Fun = fun () -> mnesia:index_read(imersia_admin_channels, Name, #channel.name) end,
    ChannelList = mnesia:transaction(Fun),

    case ChannelList of
        {atomic, []} ->
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
            Fun2 = fun () -> mnesia:write(imersia_admin_channels, DBEntry, write) end,
            Result = mnesia:transaction(Fun2),
            case Result of
                {atomic, ok} ->
                    _ = imersia_db:metadata_init(Connection, ChannelID),
                    _ = imersia_db:context_init(Connection, ChannelID),
                    channel_new_table(Connection, ChannelID);
                % Some miscellaneous error
                _ -> {error, database}
            end;
        {atomic, [_]} -> {error, name};
        _ -> {error, database}
    end.

channel_new_table(_Connection, ChannelID) ->
    TableName = list_to_atom(binary_to_list(<<"imersia_channels_">>) ++ binary_to_list(binary:replace(ChannelID, <<"-">>, <<"">>, [global]))),
    _ = mnesia:create_table(TableName, [
        {attributes, record_info(fields, geobot)},
        {record_name, geobot},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    _ = mnesia:add_table_index(TableName, location),
    _ = mnesia:add_table_index(TableName, modified),
    _ = mnesia:add_table_index(TableName, created),
    {ok, ChannelID}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Rename the Channel
% ----------------------------------------------------------------------------------------------------
channel_rename(Connection, ChannelID, Name) ->
    % Check that a channel with this name doesn't already exist
    Fun = fun () -> mnesia:index_read(imersia_admin_channels, Name, #channel.name) end,
    ChannelList = mnesia:transaction(Fun),
    case ChannelList of
        {atomic, []} -> do_channel_rename(Connection, ChannelID, Name);
        {atomic, [ExistingChannel | _]} ->
            if
                ExistingChannel#channel.channelid =/= ChannelID -> {error, name};
                true -> {ok, updated}
            end;

        _ -> {error, database}
    end.

do_channel_rename(_Connection, ChannelID, Name) ->
    NewRecord = #channel {
        name = Name,
        modified = iso8601:format(calendar:universal_time())
    },
    Fun2 = fun () -> mnesia:read(imersia_admin_channels, ChannelID) end,
    Result = mnesia:transaction(Fun2),
    case Result of
        {atomic, []} -> {error, channelid};
        {atomic, [OldRecord]} ->
            DBEntry = imersia_misc:meld_records(OldRecord, NewRecord),
            Fun3 = fun () -> mnesia:write(imersia_admin_channels, DBEntry, write) end,
            Result2 = mnesia:transaction(Fun3),
            case Result2 of
                {atomic, ok} ->
                    {ok, updated};
                _ -> {error, database}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the Channel ID given the Channel Name
% ----------------------------------------------------------------------------------------------------
channel_id(_Connection, undefined) -> {error, name};
channel_id(_Connection, Name) ->
    Fun = fun () -> mnesia:index_read(imersia_admin_channels, Name, #channel.name) end,
    ChannelList = mnesia:transaction(Fun),
    case ChannelList of
        {atomic, []} -> {error, name};
        {atomic, [Channel | _]} ->
            {ok, Channel#channel.channelid};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this channel exists
% ----------------------------------------------------------------------------------------------------
channel_exists(_Connection, undefined) -> {error, channelid};
channel_exists(_Connection, ChannelID) ->
    Fun = fun () -> mnesia:read(imersia_admin_channels, ChannelID) end,
    Channel = mnesia:transaction(Fun),

    case Channel of
        {atomic, []} -> {error, channelid};
        {atomic, [_]} -> {ok, exists};
        {_, _} -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the channels for this User
% ----------------------------------------------------------------------------------------------------
channel_list(Connection, UserID, VisitorID, ShowHidden) ->
    channel_list(Connection, UserID, VisitorID, ShowHidden, []).

channel_list(Connection, undefined, undefined, ShowHidden, ContextIDs) ->
    AllRecords = fun (Record, Acc) -> [Record | Acc] end,
    Fun = fun() -> mnesia:foldl(AllRecords, [], imersia_admin_channels) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, ChannelList} ->
            {ok, process_channel_list(Connection, ChannelList, undefined, undefined, ShowHidden, ContextIDs)};
        _Error -> imersia_misc:debug(error, "Error reading channels in Velocity Engine~n", []), {error, database}
    end;

channel_list(Connection, UserID, VisitorID, ShowHidden, ContextIDs) ->
    Fun = fun () -> mnesia:index_read(imersia_admin_channels, UserID, #channel.ownerid) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, ChannelList} ->
            {ok, process_channel_list(Connection, ChannelList, UserID, VisitorID, ShowHidden, ContextIDs)};
        _Error -> imersia_misc:debug(error, "Error reading channels in Velocity Engine~n", []), {error, database}
    end.

% channel_list(Connection, UserID, VisitorID, ShowHidden, Location, Radius, ContextIDs) ->
%     case imersia_db:geobot_list(Connection, undefined, VisitorID, Location, Radius, ShowHidden) of
%         {ok, Geobots} ->
%             ChannelIDs = extract_channelids(Geobots),
%             ChannelList = get_channels(ChannelIDs, VisitorID, ShowHidden, ContextIDs),
%             {ok, ChannelList};
%         {error, Reason} -> {error, Reason}
%     end.


% Get List of IDs for the Velocity Engine
channel_list(_Connection) ->
    AllRecords = fun (Record, Acc) -> [Record | Acc] end,
    Fun = fun() -> mnesia:foldl(AllRecords, [], imersia_admin_channels) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, ChannelList} -> {ok, extract_ids(ChannelList)};
        _Error -> imersia_misc:debug(error, "Error reading channels in Velocity Engine~n", []), {error, database}
    end.

extract_ids([]) -> [];
extract_ids([Channel | Channels]) -> [Channel#channel.channelid | extract_ids(Channels)].

process_channel_list(_, [], _, _, _, _) -> [];
process_channel_list(Connection, [ChannelRecord | Channels], UserID, VisitorID, ShowHidden, ContextIDs) ->
    IsAllowed = imersia_db:context_is_allowed(Connection, ChannelRecord#channel.channelid, ContextIDs, <<"read">>),
    if
        ((ShowHidden and ChannelRecord#channel.hidden and (VisitorID == ChannelRecord#channel.ownerid)) or
        (not ChannelRecord#channel.hidden) or
        IsAllowed) ->
            [ChannelRecord | process_channel_list(Connection, Channels, UserID, VisitorID, ShowHidden, ContextIDs)];
        true ->
            process_channel_list(Connection, Channels, UserID, VisitorID, ShowHidden, ContextIDs)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the details of the given channel
% ----------------------------------------------------------------------------------------------------
channel_getdetails(_Connection, ChannelID) ->
    Fun = fun () -> mnesia:read(imersia_admin_channels, ChannelID) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, []} -> {error, channelid};
        {atomic, [Channel]} -> {ok, Channel};
        _ -> {error, database}
    end.

channel_getdetails(_Connection, ChannelID, VisitorID) ->
    Fun = fun () -> mnesia:read(imersia_admin_channels, ChannelID) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, []} -> {error, channelid};
        {atomic, [Channel]} ->
            if
                ((VisitorID == Channel#channel.ownerid) or (not Channel#channel.hidden)) ->
                    {ok, Channel};
                true ->
                    {error, channelid}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set the details for the given channel (except the name)
% ----------------------------------------------------------------------------------------------------
channel_setdetails(_Connection, ChannelID, Details) ->
    NewRecord = Details#channel{name=null, modified=iso8601:format(calendar:universal_time())},
    Fun = fun () -> mnesia:read(imersia_admin_channels, ChannelID) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> {error, channelid};
        {atomic, [OldRecord]} ->
            DBEntry = imersia_misc:meld_records(OldRecord, NewRecord),
            Fun2 = fun () -> mnesia:write(imersia_admin_channels, DBEntry, write) end,
            Result2 = mnesia:transaction(Fun2),
            case Result2 of
                {atomic, ok} ->
                    {ok, updated};
                _ -> {error, database}
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
            case imersia_db:metadata_drop(Connection, ChannelID) of
                {ok, deleted} ->
                    Fun = fun () -> mnesia:delete({imersia_admin_channels, ChannelID}) end,
                    Result = mnesia:transaction(Fun),
                    case Result of
                        {atomic, ok} ->
                            TableName = list_to_atom(binary_to_list(<<"imersia_channels_">>) ++ binary_to_list(binary:replace(ChannelID, <<"-">>, <<"">>, [global]))),
                            Result2 = mnesia:delete_table(TableName),
                            case Result2 of
                                {atomic, ok} -> {ok, deleted};
                                _ -> {error, database}
                            end;
                        _ -> {error, channelid}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Channels for this user
% ----------------------------------------------------------------------------------------------------
channel_delete_all(Connection, UserID) ->
    Fun = fun () -> mnesia:index_read(imersia_admin_channels, UserID, #channel.ownerid) end,
    Result = mnesia:transaction(Fun),

    case Result of
        {atomic, ChannelList} -> process_channel_delete(Connection, ChannelList, UserID);
        _ -> {error, database}
    end.

process_channel_delete(_, [], _) -> {ok, deleted};
process_channel_delete(Connection, [Channel | Channels], UserID) ->
    case imersia_db:channel_delete(Connection, Channel#channel.channelid) of
        {ok, deleted} -> process_channel_delete(Connection, Channels, UserID);
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
