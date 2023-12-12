% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Context functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_context).

-include("../imersia_datatypes.hrl").

-export([
    context_init/2, context_exists/3, context_drop/2, context_list/2, context_get/3, context_set/4, context_delete/3, context_delete_all/2
]).



% ----------------------------------------------------------------------------------------------------
% Set up the table for this object ID
% ----------------------------------------------------------------------------------------------------
context_init(_Connection, ID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            mc_worker_api:disconnect(Connection),
            {ok, created};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check whether this Context exists
% ----------------------------------------------------------------------------------------------------
context_exists(_Connection, ID, ContextID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"context">>, #{<<"_id">> => ContextID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, contextid};
                _Details ->
                    mc_worker_api:disconnect(Connection),
                    {ok, exists}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the Contexts for this ID
% ----------------------------------------------------------------------------------------------------
context_list(_Connection, ID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find(Connection, <<"context">>, {}) of
                {ok, Cursor} ->
                    ContextList = get_all(Cursor),
                    mc_cursor:close(Cursor),
                    mc_worker_api:disconnect(Connection),
                    {ok, ContextList};
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {ok, []}
            end;
        _ -> {error, database}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {ContextMap}) ->
    Context = #context {
        contextid = maps:get(<<"_id">>, ContextMap),
        attributes = maps:get(<<"attributes">>, ContextMap)
    },
    [Context | get_at_cursor(Cursor, mc_cursor:next(Cursor))].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the Attributes of the given ContextID
% ----------------------------------------------------------------------------------------------------
context_get(_Connection, ID, ContextID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"context">>, #{<<"_id">> => ContextID}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, contextid};
                Details ->
                    mc_worker_api:disconnect(Connection),
                    Context = #context {
                        contextid = maps:get(<<"_id">>, Details),
                        attributes = maps:get(<<"attributes">>, Details)
                    },
                    {ok, Context}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create new or update existing Context Attributes
% ----------------------------------------------------------------------------------------------------
context_set(_Connection, ID, undefined, Attributes) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            ContextID = imersia_db:new_id(),
            DBEntry = #{
                <<"_id">> => ContextID,
                <<"attributes">> => Attributes
            },
            mc_worker_api:insert(Connection, <<"context">>, DBEntry),
            mc_worker_api:disconnect(Connection),
            {ok, ContextID};
        _ -> {error, database}
    end;
context_set(_Connection, ID, ContextID, Attributes) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"context">>, #{<<"_id">> => ContextID}) of
                undefined ->
                    ContextID2 = imersia_db:new_id(),
                    DBEntry = #{
                        <<"_id">> => ContextID2,
                        <<"attributes">> => Attributes
                    },
                    mc_worker_api:insert(Connection, <<"context">>, DBEntry),
                    mc_worker_api:disconnect(Connection),
                    {ok, ContextID2};
                OldContext ->
                    ContextID = maps:get(<<"_id">>, OldContext),

                    DBEntry = #{
                        <<"_id">> => ContextID,
                        <<"attributes">> => Attributes
                    },
                    Command = #{<<"$set">> => DBEntry},
                    mc_worker_api:update(Connection, <<"context">>, #{<<"_id">> => ContextID}, Command),
                    mc_worker_api:disconnect(Connection),
                    {ok, ContextID}
            end;
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the Context
% ----------------------------------------------------------------------------------------------------
context_delete(_Connection, ID, ContextID) ->
    case mc_worker_api:connect ([{database, <<"imersia_", ID/binary>>}]) of
        {ok, Connection2} ->
            mc_worker_api:delete(Connection2, <<"context">>, #{<<"_id">> => ContextID}),
            mc_worker_api:disconnect(Connection2),
            {ok, deleted};
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
% Not required for MongoDB, but inmcluded for completeness
% ----------------------------------------------------------------------------------------------------
context_drop(_Connection, _ID) -> {ok, deleted}.
% ----------------------------------------------------------------------------------------------------
