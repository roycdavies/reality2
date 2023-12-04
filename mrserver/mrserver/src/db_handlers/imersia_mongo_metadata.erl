% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Metadata functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_metadata).

-include("../imersia_datatypes.hrl").

-export([
    metadata_init/2, metadata_exists/3, metadata_drop/2, metadata_id/3, metadata_list/2, metadata_get/3, metadata_set/4, metadata_delete/3, metadata_get_by_id/3, metadata_set_by_id/4, metadata_delete_by_id/3, metadata_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Set up the table for this object ID
% ----------------------------------------------------------------------------------------------------
metadata_init(_Connection, ID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            mc_worker_api:ensure_index(Connection, <<"metadata">>, #{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"key">>}),
            mc_worker_api:disconnect(Connection),
            {ok, created};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Metadata exists
% ----------------------------------------------------------------------------------------------------
metadata_exists(_Connection, ID, MetadataID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"_id">> => MetadataID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, metadataid};
                _Details ->
                    mc_worker_api:disconnect(Connection),
                    {ok, exists}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the MetadataID given the Key, if it exists
% ----------------------------------------------------------------------------------------------------
metadata_id(_, _, undefined) -> {error, key};
metadata_id(_, undefined, _) -> {error, id};
metadata_id(_Connection, ID, Key) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"key">> => Key}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, metadataid};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    {ok, maps:get(<<"_id">>, Details)}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Metadata for this ID
% ----------------------------------------------------------------------------------------------------
metadata_list(_Connection, ID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"metadata">>, {}) of
                {ok, Cursor} ->
                    MetadataList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, MetadataList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {MetadataMap}) ->
    Metadata = #metadata {
        metadataid = maps:get(<<"_id">>, MetadataMap),
        key = maps:get(<<"key">>, MetadataMap),
        value = maps:get(<<"value">>, MetadataMap)
    },
    [Metadata | get_at_cursor(Cursor, mc_cursor:next(Cursor))].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Value of the given metadata Key
% ----------------------------------------------------------------------------------------------------
metadata_get(_Connection, ID, Key) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"key">> => Key}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, key};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    Metadata = #metadata {
                        metadataid = maps:get(<<"_id">>, Details),
                        key = maps:get(<<"key">>, Details),
                        value = maps:get(<<"value">>, Details)
                    },
                    {ok, Metadata}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Value of the given metadata MetadataID
% ----------------------------------------------------------------------------------------------------
metadata_get_by_id(_Connection, ID, MetadataID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"_id">> => MetadataID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, metadataid};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    Metadata = #metadata {
                        metadataid = maps:get(<<"_id">>, Details),
                        key = maps:get(<<"key">>, Details),
                        value = maps:get(<<"value">>, Details)
                    },
                    {ok, Metadata}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_set(_Connection, ID, Key, Value) ->
    CheckNull = fun(_Key, Val) -> (Val /= null) and (Val /= undefined) end,
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"key">> => Key}) of
                undefined ->
                    MetadataID = imersia_db:new_id(),
                    ValueMap = imersia_misc:safely_convert_to_map(Value),

                    DBEntry = maps:filter(CheckNull, #{
                        <<"_id">> => MetadataID,
                        <<"key">> => choose_one(<<"">>, Key),
                        <<"value">> => choose_one(<<"">>, ValueMap)
                    }),
                    mc_worker_api:insert(Connection, <<"metadata">>, DBEntry),
                    mc_worker_api:disconnect(Connection),
                    {ok, MetadataID};
                OldMetadata ->
                    MetadataID = maps:get(<<"_id">>, OldMetadata),

                    ValueMap = imersia_misc:safely_convert_to_map(Value),
                    OldValueMap = imersia_misc:safely_convert_to_map(maps:get(<<"value">>, OldMetadata)),
                    DBEntry = maps:filter(CheckNull, #{
                        <<"_id">> => MetadataID,
                        <<"key">> => choose_one(maps:get(<<"key">>, OldMetadata), Key),
                        <<"value">> => choose_one(OldValueMap, ValueMap)
                    }),
                    Command = #{<<"$set">> => DBEntry},
                    mc_worker_api:update(Connection, <<"metadata">>, #{<<"_id">> => MetadataID}, Command),
                    mc_worker_api:disconnect(Connection),
                    {ok, MetadataID}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Update an existing metadata Key and/or Value by ID
% ----------------------------------------------------------------------------------------------------
metadata_set_by_id(Connection, ID, MetadataID, NewMetadata) ->
    case imersia_db:metadata_get_by_id(Connection, ID, MetadataID) of
        {ok, OldMetadata} ->
            case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
                {ok, Connection2} ->
                    CheckNull = fun(_Key, Value) -> (Value /= null) and (Value /= undefined) end,

                    ValueMap = imersia_misc:safely_convert_to_map(NewMetadata#metadata.value),
                    OldValueMap = imersia_misc:safely_convert_to_map(OldMetadata#metadata.value),
                    DBEntry = maps:filter(CheckNull, #{
                        <<"_id">> => MetadataID,
                        <<"key">> => choose_one(OldMetadata#metadata.key, NewMetadata#metadata.key),
                        <<"value">> => choose_one(OldValueMap, ValueMap)
                    }),
                    Command = #{<<"$set">> => DBEntry},

                    mc_worker_api:update(Connection2, <<"metadata">>, #{<<"_id">> => MetadataID}, Command),
                    mc_worker_api:disconnect(Connection2),
                    {ok, updated};
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.

choose_one(undefined, Value) -> Value;
choose_one(null, Value) -> Value;
choose_one(Value, undefined) -> Value;
choose_one(Value, null) -> Value;
choose_one(_OldValue, NewValue) -> NewValue.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_delete(_Connection, ID, Key) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"metadata">>, #{<<"key">> => Key}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, key};
                Details ->
                    imersia_db:metadata_delete_by_id(Connection, ID, maps:get(<<"_id">>, Details)),
                    mc_worker_api:disconnect(Connection),
                    {ok, deleted}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_delete_by_id(_Connection, ID, MetadataID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection2} ->
            mc_worker_api:delete(Connection2, <<"metadata">>, #{<<"_id">> => MetadataID}),
            mc_worker_api:disconnect(Connection2),
            {ok, deleted};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Metadata for this ID
% ----------------------------------------------------------------------------------------------------
metadata_delete_all(Connection, ID) ->
    case imersia_db:metadata_list(Connection, ID) of
        {ok, MetadataList} -> delete_metadata_list(Connection, ID, MetadataList);
        {error, Reason} -> {error, Reason}
    end.

delete_metadata_list(_, _, []) -> {ok, deleted};
delete_metadata_list(Connection, ID, [Head | Tail]) ->
    imersia_db:metadata_delete_by_id(Connection, ID, Head#metadata.metadataid),
    delete_metadata_list(Connection, ID, Tail).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Not required for MongoDB, but inmcluded for completeness
% ----------------------------------------------------------------------------------------------------
metadata_drop(_Connection, _ID) -> {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
