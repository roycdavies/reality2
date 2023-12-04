% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database implementation of Passcode functions using Mnesia
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_mnesia_passcode).

-include("../imersia_datatypes.hrl").

-export([
    passcode_new/2, passcode_check/3
]).


% ----------------------------------------------------------------------------------------------------
% Create a new passcode and set the counter to 1
% ----------------------------------------------------------------------------------------------------
passcode_new(Connection, UserEmail) ->
    case delete_existing_passcodes(Connection, UserEmail) of
        {ok, deleted} ->
            case create_new_passcode(Connection, UserEmail) of
                {ok, Passcode} ->
                    case send_passcode_by_email(UserEmail, Passcode) of
                        {ok, sent} ->
                            {ok, created};
                        _ ->
                            {error, smtp}
                    end;
                _ ->
                    {error, database}
            end;
        _ ->
            {error, database}
    end.

delete_existing_passcodes(Connection, UserEmail) ->
    Fun = fun () -> mnesia:index_read(imersia_users_passcodes, UserEmail, #passcode.useremail) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> {ok, deleted};
        {atomic, PasscodeList} ->
            process_passcode_delete(Connection, PasscodeList, UserEmail);
        _ -> {error, database}
    end.

process_passcode_delete(_, [], _) -> {ok, deleted};
process_passcode_delete(Connection, [Passcode | Passcodes], UserEmail) ->
    PasscodeID = Passcode#passcode.passcodeid,
    Fun = fun () -> mnesia:delete({imersia_users_passcodes, PasscodeID}) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} ->
            process_passcode_delete(Connection, Passcodes, UserEmail);
        _ -> {error, database}
    end.

create_new_passcode(_, UserEmail) ->
    PasscodeID = imersia_db:new_id(),
    Passcode = extract_code_from_ID(PasscodeID),
    DBEntry = #passcode{
        passcodeid = PasscodeID,
        passcode = Passcode,
        useremail = UserEmail,
        created = iso8601:format(calendar:universal_time()),
        used = false,
        count = 1
    },
    Fun = fun () -> mnesia:write(imersia_users_passcodes, DBEntry, write) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, ok} -> {ok, Passcode};
        _ -> {error, database}
    end.

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
passcode_check(Connection, UserEmail, Passcode) ->
    Fun = fun () -> mnesia:index_read(imersia_users_passcodes, UserEmail, #passcode.useremail) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, []} -> {error, invalid};
        {atomic, [PasscodeEntry]} ->
            do_passcode_check_and_update(Connection, PasscodeEntry, Passcode)
    end.

do_passcode_check_and_update(Connection, PasscodeRecord, Passcode) ->
    StoredPasscode = PasscodeRecord#passcode.passcode,
    Count = PasscodeRecord#passcode.count,
    Created = PasscodeRecord#passcode.created,
    Used = PasscodeRecord#passcode.used,
    Result = passcode_logic(Passcode, StoredPasscode, Count, Used, Created),
    update_entry(Connection, Result, PasscodeRecord),
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

update_entry(_, Result, PasscodeRecord) ->
    PasscodeID = PasscodeRecord#passcode.passcodeid,
    DBEntry = #passcode{
        passcodeid = PasscodeID,
        passcode = PasscodeRecord#passcode.passcode,
        useremail = PasscodeRecord#passcode.useremail,
        count = PasscodeRecord#passcode.count + 1,
        used = if
            Result == {ok, checked} -> true;
            true -> PasscodeRecord#passcode.used
        end,
        created = PasscodeRecord#passcode.created
    },

    Fun = fun () -> mnesia:write(imersia_users_passcodes, DBEntry, write) end,
    DBResult = mnesia:transaction(Fun),
    case DBResult of
        {atomic, ok} -> {ok, updated};
        _ -> {error, database}
    end.
% ----------------------------------------------------------------------------------------------------
