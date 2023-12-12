% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Context functions using Mnesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_context).

-include("../imersia_datatypes.hrl").

-export([
    context_init/2, context_exists/3, context_drop/2, context_list/2, context_get/3, context_set/4, context_delete/3, context_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Remove the dashes from the UUID to make it a valid table name
% ----------------------------------------------------------------------------------------------------
tablename(ID) ->
    list_to_atom(binary_to_list(<<"imersia_context_">>) ++ binary_to_list(binary:replace(ID, <<"-">>, <<"">>, [global]))).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the table for this object ID
% ----------------------------------------------------------------------------------------------------
context_init(_Connection, ID) ->
    mnesia:create_table(tablename(ID), [
        {attributes, record_info(fields, context)},
        {record_name, context},
        {disc_copies, [node()]},
        {storage_properties, [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}
    ]),
    {ok, created}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Context exists
% ----------------------------------------------------------------------------------------------------
context_exists(Connection, ID, ContextID) ->
    context_init(Connection, ID),
    Fun = fun () -> mnesia:read(tablename(ID), ContextID) end,
    ContextList = mnesia:transaction(Fun),

    case ContextList of
        {atomic, []} -> {error, contextid};
        {atomic, [_]} -> {ok, exists};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Contexts for this ID
% ----------------------------------------------------------------------------------------------------
context_list(Connection, ID) ->
    context_init(Connection, ID),
    Fun = fun () -> mnesia:all_keys(tablename(ID)) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ContextIDList} ->
            case process_context_list(Connection, ID, ContextIDList) of
                {error, Reason} -> {error, Reason};
                ContextList -> {ok, ContextList}
            end;
        _ -> {error, database}
    end.

process_context_list(_, _, []) -> [];
process_context_list(Connection, ID, [ContextID | ContextIDList]) ->
    case context_get(Connection, ID, ContextID) of
        {ok, Context} -> [Context | process_context_list(Connection, ID, ContextIDList)];
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Attributes of the given ContextID
% ----------------------------------------------------------------------------------------------------
context_get(Connection, ID, ContextID) ->
    context_init(Connection, ID),
    Fun = fun () -> mnesia:read(tablename(ID), ContextID) end,
    ContextList = mnesia:transaction(Fun),

    case ContextList of
        {atomic, []} -> {error, contextid};
        {atomic, [Context]} -> {ok, Context};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Context Attributes
% ----------------------------------------------------------------------------------------------------
context_set(Connection, ID, ContextID, Attributes) ->
    case imersia_db:context_get(Connection, ID, ContextID) of
        {ok, Context} -> context_update(Connection, ID, Context#context.contextid, Attributes);
        {error, contextid} -> context_update(Connection, ID, imersia_db:new_id(), Attributes);
        {error, Reason} -> {error, Reason}
    end.
context_update(_Connection, ID, ContextID, Attributes) ->
    DBEntry = #context{
        contextid = ContextID,
        attributes = Attributes
    },
    Fun = fun () -> mnesia:write(tablename(ID), DBEntry, write) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, ContextID};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Context
% ----------------------------------------------------------------------------------------------------
context_delete(Connection, ID, ContextID) ->
    context_init(Connection, ID),
    Fun = fun () -> mnesia:delete({tablename(ID), ContextID}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, deleted};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the Contexts for this ID
% ----------------------------------------------------------------------------------------------------
context_delete_all(Connection, ID) ->
    case imersia_db:context_list(Connection, ID) of
        {ok, ContextList} -> delete_context_list(Connection, ID, ContextList);
        {error, Reason} -> {error, Reason}
    end.

delete_context_list(_, _, []) -> {ok, deleted};
delete_context_list(Connection, ID, [Head | Tail]) ->
    imersia_db:context_delete(Connection, ID, Head#context.contextid),
    delete_context_list(Connection, ID, Tail).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Completely removes the context database table for this ID - only used when deleting the Geobot, Channel or User
% ----------------------------------------------------------------------------------------------------
context_drop(_Connection, ID) ->
    mnesia:delete_table(tablename(ID)), {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
