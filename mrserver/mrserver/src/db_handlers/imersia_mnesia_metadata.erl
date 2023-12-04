% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Metadata functions using Mnesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_metadata).

-include("../imersia_datatypes.hrl").

-export([
    metadata_init/2, metadata_exists/3, metadata_drop/2, metadata_id/3, metadata_list/2, metadata_get/3, metadata_set/4, metadata_delete/3, metadata_get_by_id/3, metadata_set_by_id/4, metadata_delete_by_id/3, metadata_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Remove the dashes from the UUID to make it a valid table name
% ----------------------------------------------------------------------------------------------------
tablename(ID) ->
    list_to_atom(binary_to_list(<<"imersia_metadata_">>) ++ binary_to_list(binary:replace(ID, <<"-">>, <<"">>, [global]))).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the table for this object ID
% ----------------------------------------------------------------------------------------------------
metadata_init(_Connection, ID) ->
    mnesia:create_table(tablename(ID), [
        {attributes, record_info(fields, metadata)},
        {record_name, metadata},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    mnesia:add_table_index(tablename(ID), key),
    {ok, created}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Metadata exists
% ----------------------------------------------------------------------------------------------------
metadata_exists(_Connection, ID, MetadataID) ->
    Fun = fun () -> mnesia:read(tablename(ID), MetadataID) end,
    MetadataList = mnesia:transaction(Fun),

    case MetadataList of
        {atomic, []} -> {error, metadataid};
        {atomic, [_]} -> {ok, exists};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the MetadataID given the Key, if it exists
% ----------------------------------------------------------------------------------------------------
metadata_id(_, _, undefined) -> {error, key};
metadata_id(_, undefined, _) -> {error, id};
metadata_id(_Connection, ID, Key) ->
    Fun = fun () -> mnesia:index_read(tablename(ID), Key, #metadata.key) end,
    MetadataList = mnesia:transaction(Fun),

    case MetadataList of
        {atomic, []} -> {error, key};
        {atomic, [Metadata | _]} -> {ok, Metadata#metadata.metadataid};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Metadata for this ID
% ----------------------------------------------------------------------------------------------------
metadata_list(Connection, ID) ->
    Fun = fun () -> mnesia:all_keys(tablename(ID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, MetadataIDList} ->
            case process_metadata_list(Connection, ID, MetadataIDList) of
                {error, Reason} -> {error, Reason};
                MetadataList -> {ok, MetadataList}
            end;
        _ -> {error, database}
    end.

process_metadata_list(_, _, []) -> [];
process_metadata_list(Connection, ID, [MetadataID | MetadataIDList]) ->
    case metadata_get_by_id(Connection, ID, MetadataID) of
        {ok, Metadata} -> [Metadata | process_metadata_list(Connection, ID, MetadataIDList)];
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Value of the given metadata Key
% ----------------------------------------------------------------------------------------------------
metadata_get(_Connection, ID, Key) ->
    Fun = fun () -> mnesia:index_read(tablename(ID), Key, #metadata.key) end,
    MetadataList = mnesia:transaction(Fun),

    case MetadataList of
        {atomic, []} -> {error, key};
        {atomic, [Metadata | _]} -> {ok, Metadata};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Value of the given metadata MetadataID
% ----------------------------------------------------------------------------------------------------
metadata_get_by_id(_Connection, ID, MetadataID) ->
    Fun = fun () -> mnesia:read(tablename(ID), MetadataID) end,
    MetadataList = mnesia:transaction(Fun),

    case MetadataList of
        {atomic, []} -> {error, metadataid};
        {atomic, [Metadata]} -> {ok, Metadata};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_set(Connection, ID, Key, Value) ->
    case imersia_db:metadata_get(Connection, ID, Key) of
        {ok, Metadata} -> metadata_update(Connection, ID, Metadata#metadata.metadataid, Key, Value);
        {error, key} -> metadata_update(Connection, ID, imersia_db:new_id(), Key, Value);
        {error, Reason} -> {error, Reason}
    end.
metadata_update(_Connection, ID, MetadataID, Key, Value) ->
    DBEntry = #metadata{
        metadataid = MetadataID,
        key = Key,
        value = Value
    },
    Fun = fun () -> mnesia:write(tablename(ID), DBEntry, write) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, MetadataID};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Update an existing metadata Key and/or Value by ID
% ----------------------------------------------------------------------------------------------------
metadata_set_by_id(Connection, ID, MetadataID, NewMetadata) ->
    case  imersia_db:metadata_get_by_id(Connection, ID, MetadataID) of
        {ok, OldMetadata} ->
            DBEntry = imersia_misc:meld_records(OldMetadata, NewMetadata),
            Fun = fun () -> mnesia:write(tablename(ID), DBEntry, write) end,
            Result = mnesia:transaction(Fun),
            case Result of
                {atomic, ok} -> {ok, updated};
                _ -> {error, database}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_delete(Connection, ID, Key) ->
    Fun = fun () -> mnesia:index_read(tablename(ID), Key, #metadata.key) end,
    MetadataList = mnesia:transaction(Fun),

    case MetadataList of
        {atomic, []} -> {error, key};
        {atomic, [Metadata | _]} -> imersia_db:metadata_delete_by_id(Connection, ID, Metadata#metadata.metadataid);
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Metadata Key/Value
% ----------------------------------------------------------------------------------------------------
metadata_delete_by_id(_Connection, ID, MetadataID) ->
    Fun = fun () -> mnesia:delete({tablename(ID), MetadataID}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, deleted};
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
% Completely removes the metadata database table for this ID - only used when deleting the Geobot, Channel or User
% ----------------------------------------------------------------------------------------------------
metadata_drop(_Connection, ID) ->
    mnesia:delete_table(tablename(ID)), {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
