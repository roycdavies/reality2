% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Passcode functions using MongoDB
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mongo_passcode).

-include("../imersia_datatypes.hrl").

-export([
    passcode_new/2, passcode_check/3
]).


% ----------------------------------------------------------------------------------------------------
% Create a new passcode and set the counter to 1
% ----------------------------------------------------------------------------------------------------
passcode_new(_, UserEmail) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            case delete_existing_passcodes(Connection, UserEmail) of
                {ok, deleted} ->
                    case create_new_passcode(Connection, UserEmail) of
                        {ok, Passcode} ->
                            case send_passcode_by_email(UserEmail, Passcode) of
                                {ok, sent} ->
                                    mc_worker_api:disconnect(Connection),
                                    {ok, created};
                                _ ->
                                    mc_worker_api:disconnect(Connection),
                                    {error, smtp}
                            end;
                        _ ->
                            mc_worker_api:disconnect(Connection),
                            {error, database}
                    end;
                _ ->
                    mc_worker_api:disconnect(Connection),
                    {error, database}
            end;
        _ ->
            {error, database}
    end.

delete_existing_passcodes(Connection, UserEmail) ->
    case mc_worker_api:find(Connection, <<"passcodes">>, #{<<"useremail">> => UserEmail}) of
        {ok, Cursor} ->
            PasscodeIDList = get_all(Cursor),
            mc_cursor:close(Cursor),
            process_passcode_delete(Connection, PasscodeIDList, UserEmail);
        _ ->
            {ok, deleted}
    end.

get_all(Cursor) -> get_at_cursor(Cursor, mc_cursor:next(Cursor)).
get_at_cursor(_, error) -> [];
get_at_cursor(Cursor, {PasscodeMap}) ->
    PasscodeID = maps:get(<<"_id">>, PasscodeMap),
    [PasscodeID | get_at_cursor(Cursor, mc_cursor:next(Cursor))].

process_passcode_delete(_, [], _) -> {ok, deleted};
process_passcode_delete(Connection, [PasscodeID | PasscodeIDs], UserEmail) ->
    mc_worker_api:delete(Connection, <<"passcodes">>, #{<<"_id">> => PasscodeID}),
    process_passcode_delete(Connection, PasscodeIDs, UserEmail).

create_new_passcode(Connection, UserEmail) ->
    PasscodeID = imersia_db:new_id(),
    Passcode = extract_code_from_ID(PasscodeID),
    DBEntry = #{
        <<"_id">> => PasscodeID,
        <<"passcode">> => Passcode,
        <<"useremail">> => UserEmail,
        <<"created">> => iso8601:format(calendar:universal_time()),
        <<"used">> => false,
        <<"count">> => 1
    },
    mc_worker_api:insert(Connection, <<"passcodes">>, DBEntry),
    {ok, Passcode}.

extract_code_from_ID(PasscodeID) ->
    <<Passcode:6/binary, _/binary>> = PasscodeID,
    string:uppercase(Passcode).

send_passcode_by_email(UserEmail, Passcode) ->
    ServerName = imersia_settings:get_setting(mrserver, url),
    Subject = <<Passcode/binary, " is your single-use passcode for ", ServerName/binary>>,
    Message = <<"Your single-use passcode is ", Passcode/binary, " on ", ServerName/binary, ".  If you weren't expecting this then it means someone else is trying to access your account.">>,
    imersia_smtp:send_email(UserEmail, Subject, Message).
% ----------------------------------------------------------------------------------------------------

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
passcode_check(_, UserEmail, Passcode) ->
    case mc_worker_api:connect ([{database, <<"imersia_admin">>}]) of
        {ok, Connection} ->
            case mc_worker_api:find_one(Connection, <<"passcodes">>, #{<<"useremail">> => UserEmail}) of
                undefined ->
                    mc_worker_api:disconnect(Connection),
                    {error, invalid};
                PasscodeEntry ->
                    Result = do_passcode_check_and_update(Connection, PasscodeEntry, Passcode),
                    mc_worker_api:disconnect(Connection),
                    Result
            end;
        _ ->
            {error, database}
    end.

do_passcode_check_and_update(Connection, PasscodeMap, Passcode) ->
    StoredPasscode = maps:get(<<"passcode">>, PasscodeMap),
    Count = maps:get(<<"count">>, PasscodeMap),
    Created = maps:get(<<"created">>, PasscodeMap),
    Used = maps:get(<<"used">>, PasscodeMap),
    Result = passcode_logic(Passcode, StoredPasscode, Count, Used, Created),
    update_entry(Connection, Result, PasscodeMap),
    Result.

passcode_logic(Passcode, Passcode, Count, Used, Created) ->
    passcode_used(Count, Used, Created);
passcode_logic(_, _, _, _, _) ->
    {error, invalid}.

passcode_used(Count, false, Created) ->
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds(iso8601:parse(Created)),
    passcode_timedout(Count, TimeDiff);
passcode_used(_, true, _) ->
    {error, used}.

passcode_timedout(Count, TimeDiff) when TimeDiff < ?PASSCODE_EXP_TIME ->
    passcode_count(Count);
passcode_timedout(_, _) ->
    {error, timeout}.

passcode_count(Count) when Count =< ?PASSCODE_MAX_NUMTRIES ->
    {ok, checked};
passcode_count(_) ->
    {error, mumtries}.

update_entry(Connection, Result, PasscodeMap) ->
    PasscodeID = maps:get(<<"_id">>, PasscodeMap),
    DBEntry = #{
        <<"_id">> => PasscodeID,
        <<"count">> => maps:get(<<"count">>, PasscodeMap) + 1,
        <<"used">> => if
            Result =:= {ok, checked} -> true;
            true -> maps:get(<<"used">>, PasscodeMap)
        end
    },
    Command = #{<<"$set">> => DBEntry},
    mc_worker_api:update(Connection, <<"passcodes">>, #{<<"_id">> => PasscodeID}, Command).
% ----------------------------------------------------------------------------------------------------
