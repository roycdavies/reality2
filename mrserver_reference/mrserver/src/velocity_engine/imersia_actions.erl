% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The core of the Velocity Engine, managing Automations, Transitions and Actions
% ----------------------------------------------------------------------------------------------------

-module(imersia_actions).

-export([interpret/4, clear_timers/1]).

-include("../imersia_datatypes.hrl").


% ----------------------------------------------------------------------------------------------------
% Interpret the commands coming in and send them off to the appropriate handlers
% ----------------------------------------------------------------------------------------------------
interpret(GeobotID, <<"log">>, Parameters, EventParameters) ->
    Event = ej:get({<<"event">>}, Parameters),
    ProcessedEvent = process_expression({GeobotID, undefined, EventParameters}, Event, str),
    LogParameters = ej:get({<<"parameters">>}, Parameters),
    action_log(GeobotID, ProcessedEvent, compound_parameters(GeobotID, LogParameters, EventParameters));

interpret(GeobotID, <<"set">>, Parameters, EventParameters) ->
    Key = ej:get({<<"key">>}, Parameters),
    Value = ej:get({<<"value">>}, Parameters),
    ProcessedValue = process_expression({GeobotID, Key, EventParameters}, Value, str),
    action_set(GeobotID, Key, EventParameters, ProcessedValue);

interpret(GeobotID, <<"send">>, Parameters, EventParameters) ->
    DestinationID = ej:get({<<"geobotid">>}, Parameters),
    URL = ej:get({<<"url">>}, Parameters),
    Event = ej:get({<<"event">>}, Parameters),
    Delay = ej:get({<<"delay">>}, Parameters),
    SendParameters = ej:get({<<"parameters">>}, Parameters),
    ProcessedURL = process_expression({GeobotID, undefined, EventParameters}, URL, str),
    ProcessedDestinationID = select_parameter(GeobotID, process_expression({GeobotID, undefined, EventParameters}, DestinationID, str)),
    ProcessedEvent = process_expression({GeobotID, undefined, EventParameters}, Event, str),
    ProcessedDelay = process_expression({GeobotID, undefined, EventParameters}, Delay, num),
    action_send(GeobotID, ProcessedURL, ProcessedDestinationID, ProcessedEvent, ProcessedDelay, compound_parameters(GeobotID, SendParameters, EventParameters));

interpret(GeobotID, <<"email">>, Parameters, EventParameters) ->
    Recipients = ej:get({<<"recipients">>}, Parameters),
    Subject = ej:get({<<"subject">>}, Parameters),
    Message = ej:get({<<"message">>}, Parameters),
    ProcessedRecipients = process_expression({GeobotID, undefined, EventParameters}, Recipients, str),
    ProcessedSubject = process_expression({GeobotID, undefined, EventParameters}, Subject, str),
    ProcessedMessage = process_expression({GeobotID, undefined, EventParameters}, Message, str),
    action_email(GeobotID, ProcessedRecipients, ProcessedSubject, ProcessedMessage);

interpret(GeobotID, <<"test">>, Parameters, EventParameters) ->
    DestinationID = ej:get({<<"geobotid">>}, Parameters),
    Test = ej:get({<<"test">>}, Parameters),
    TrueEvent = ej:get({<<"true_event">>}, Parameters),
    FalseEvent = ej:get({<<"false_event">>}, Parameters),
    ProcessedTest = process_expression({GeobotID, undefined, EventParameters}, Test, err),
    ProcessedTrueEvent = process_expression({GeobotID, undefined, EventParameters}, TrueEvent, str),
    ProcessedFalseEvent = process_expression({GeobotID, undefined, EventParameters}, FalseEvent, str),
    action_test(GeobotID, DestinationID, ProcessedTest, ProcessedTrueEvent, ProcessedFalseEvent);

interpret(GeobotID, <<"broadcast">>, Parameters, EventParameters) ->
    ChannelID = ej:get({<<"channelid">>}, Parameters),
    Radius = ej:get({<<"radius">>}, Parameters),
    Event = ej:get({<<"event">>}, Parameters),
    Delay = ej:get({<<"delay">>}, Parameters),
    SendParameters = ej:get({<<"parameters">>}, Parameters),
    ProcessedChannelID = select_parameter(ChannelID, process_expression({GeobotID, undefined, EventParameters}, ChannelID, str)),
    ProcessedRadius = process_expression({GeobotID, undefined, EventParameters}, Radius, num),
    ProcessedEvent = process_expression({GeobotID, undefined, EventParameters}, Event, str),
    ProcessedDelay = process_expression({GeobotID, undefined, EventParameters}, Delay, num),
    action_broadcast(GeobotID, ProcessedChannelID, ProcessedRadius, ProcessedEvent, ProcessedDelay, compound_parameters(GeobotID, SendParameters, EventParameters));

interpret(GeobotID, <<"trigger">>, Parameters, EventParameters) ->
    Event = ej:get({<<"event">>}, Parameters),
    TriggerParameters = ej:get({<<"parameters">>}, Parameters),
    ProcessedEvent = process_expression({GeobotID, undefined, EventParameters}, Event, str),
    action_trigger(GeobotID, ProcessedEvent, compound_parameters(GeobotID, TriggerParameters, EventParameters));

interpret(GeobotID, <<"spend">>, Parameters, EventParameters) ->
    case ej:get({<<"userid">>}, EventParameters) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "No UserID specified for spend~n", []),
            {spend, error, userid};
        UserID ->
            % Get the user's email address from the userid
            Connection = imersia_db:connect(),
            case imersia_db:user_email(Connection, UserID) of
                {ok, UserEmail} ->
                    imersia_db:close(Connection),

                    % Get the number of tokens to spend, or 1 if not defined
                    NumTokens = case ej:get({<<"tokens">>}, Parameters) of
                        undefined -> 1;
                        Tokens -> process_expression({GeobotID, undefined, EventParameters}, Tokens, num)
                    end,

                    % Get a description for this spend, or blank otherwise
                    Message = case ej:get({<<"description">>}, Parameters) of
                        undefined -> <<>>;
                        Description -> process_expression({GeobotID, undefined, EventParameters}, Description, str)
                    end,

                    % Get this server's address (as set in the mrserver.toml file)
                    URL = imersia_settings:get_setting(mrserver, url),

                    % Compose a message that includes the GeobotID, the number of tokens, and the message
                    CompleteMessage = imersia_misc:safely_encode_json([{<<"geobotid">>, GeobotID}, {<<"url">>, URL}, {<<"numtokens">>, NumTokens}, {<<"description">>, Message}]),

                    % Attempt to spend the user's tokens
                    case imersia_billing:use_tokens(UserEmail, UserID, NumTokens, CompleteMessage) of
                        {ok, _TokensLeft} ->
                            imersia_misc:debug_to_logfile(GeobotID, debug, "~p Tokens spent for ~s~n", [NumTokens, UserEmail]),
                            {spend, ok};
                        {error, _} ->
                            imersia_misc:debug_to_logfile(GeobotID, error, "Error spending Tokens for ~s~n", [UserEmail]),
                            {spend, error, spending}
                    end;
                _ ->
                    imersia_misc:debug_to_logfile(GeobotID, error, "Error spending Tokens - user does not exist on this system~n", []),
                    {spend, error, user}
            end
    end;

interpret(GeobotID, <<"entangle">>, Parameters, EventParameters) ->
    RemoteServerAddr = ej:get({<<"url">>}, Parameters),
    RemoteUserEmail = ej:get({<<"useremail">>}, Parameters),
    RemotePassword = ej:get({<<"password">>}, Parameters),
    RemoteGeobotID = ej:get({<<"geobotid">>}, Parameters),
    RemoteDeveloperID = ej:get({<<"developerid">>}, Parameters),
    ProcessedRemoteServerAddr = process_expression({GeobotID, undefined, EventParameters}, RemoteServerAddr, str),
    ProcessedRemoteUserEmail = process_expression({GeobotID, undefined, EventParameters}, RemoteUserEmail, str),
    ProcessedRemotePassword = process_expression({GeobotID, undefined, EventParameters}, RemotePassword, str),
    ProcessedRemoteGeobotID = process_expression({GeobotID, undefined, EventParameters}, RemoteGeobotID, str),
    ProcessedRemoteDeveloperID = process_expression({GeobotID, undefined, EventParameters}, RemoteDeveloperID, str),
    action_entangle(GeobotID, ProcessedRemoteServerAddr, ProcessedRemoteDeveloperID, ProcessedRemoteUserEmail, ProcessedRemotePassword, ProcessedRemoteGeobotID);

interpret(GeobotID, <<"disentangle">>, Parameters, EventParameters) ->
    RemoteServerAddr = ej:get({<<"url">>}, Parameters),
    RemoteGeobotID = ej:get({<<"geobotid">>}, Parameters),
    RemoteDeveloperID = ej:get({<<"developerid">>}, Parameters),
    ProcessedRemoteServerAddr = process_expression({GeobotID, undefined, EventParameters}, RemoteServerAddr, str),
    ProcessedRemoteGeobotID = process_expression({GeobotID, undefined, EventParameters}, RemoteGeobotID, str),
    ProcessedRemoteDeveloperID = process_expression({GeobotID, undefined, EventParameters}, RemoteDeveloperID, str),
    action_disentangle(GeobotID, ProcessedRemoteServerAddr, ProcessedRemoteDeveloperID, ProcessedRemoteGeobotID);

interpret(GeobotID, <<"load">>, Parameters, EventParameters) ->
    Name = ej:get({<<"name">>}, Parameters),
    EntityIDGeobot = ej:get({<<"geobotid">>}, Parameters),
    EntityIDChannel = ej:get({<<"channelid">>}, Parameters),
    ContextIDs = ej:get({<<"contextids">>}, Parameters),
    {EntityType, EntityID} = geobot_or_channel(EntityIDGeobot, EntityIDChannel),
    UserID = ej:get({<<"userid">>}, EventParameters),
    ProcessedEntityID = process_expression({GeobotID, undefined, EventParameters}, EntityID, str),
    action_load(GeobotID, UserID, ProcessedEntityID, EntityType, ContextIDs, Name);

interpret(GeobotID, <<"save">>, Parameters, EventParameters) ->
    Name = ej:get({<<"name">>}, Parameters),
    EntityIDGeobot = ej:get({<<"geobotid">>}, Parameters),
    EntityIDChannel = ej:get({<<"channelid">>}, Parameters),
    ContextIDs = ej:get({<<"contextids">>}, Parameters),
    {EntityType, EntityID} = geobot_or_channel(EntityIDGeobot, EntityIDChannel),
    UserID = ej:get({<<"userid">>}, EventParameters),
    ProcessedEntityID = process_expression({GeobotID, undefined, EventParameters}, EntityID, str),
    action_save(GeobotID, UserID, ProcessedEntityID, EntityType, ContextIDs, Name);

interpret(GeobotID, Command, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Command ~p does not exist~n", [Command]),
    {command, error, illegal_command}.

geobot_or_channel(A, undefined) -> {geobot, A};
geobot_or_channel(A, <<>>) -> {geobot, A};
geobot_or_channel(undefined, B) -> {channel, B};
geobot_or_channel(<<>>, B) -> {channel, B};
geobot_or_channel(A, _) -> {geobot, A}.

% Add two lists of parameters together, giving precedence to the first list and interpreting the elements
compound_parameters(GeobotID, A, B) when is_list(A) and is_list(B) ->
    try A ++ B of
        C -> interpret_parameters(GeobotID, C)
    catch
        _:_ -> interpret_parameters(GeobotID, A)
    end;
compound_parameters(GeobotID, A, _) when is_list (A) -> interpret_parameters(GeobotID, A);
compound_parameters(_, _, _) -> [].

% Process values in a list of {key, value} tuples
interpret_parameters(_, []) -> [];
interpret_parameters(_, {}) -> [];
interpret_parameters(_, [{}]) -> [];
interpret_parameters(GeobotID, [{Key, Value} | T]) -> [{Key, process_expression({GeobotID, Key, T}, Value, err)} | interpret_parameters(GeobotID, T) ];
interpret_parameters(_, _) -> [].

select_parameter(A, undefined) -> A;
select_parameter(A, <<>>) -> A;
select_parameter(undefined, B) -> B;
select_parameter(<<>>, B) -> B;
select_parameter(_, B) -> B.

% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Log an event
% ----------------------------------------------------------------------------------------------------
action_log(GeobotID, <<>>, Parameters) -> action_log(GeobotID, undefined, Parameters);
action_log(GeobotID, "", Parameters) -> action_log(GeobotID, undefined, Parameters);
action_log(GeobotID, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error logging event - no event given~n", []),
    {log, error, event};
action_log(GeobotID, Event, Parameters) ->
    Connection = imersia_db:connect(),
    Geobot = get_geobot(GeobotID),
    Geohash = Geobot#geobot.location#location.geohash,
    Date = calendar:universal_time(),
    Result = case imersia_db:analytics_log(Connection, GeobotID, Event, Geohash, Date, Parameters) of
        {ok, _} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Logging event ~s with parameters ~p~n", [Event, Parameters]),
            {log, ok};
        {error, Reason} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error ~p logging event ~s with parameters ~p~n", [Reason, Event, Parameters]),
            {log, error, analytics_error}
    end,
    imersia_db:close(Connection),
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send an email
% ----------------------------------------------------------------------------------------------------
action_email(GeobotID, <<>>, Subject, Message) -> action_email(GeobotID, undefined, Subject, Message);
action_email(GeobotID, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Email not sent - no recipients~n", []),
    {email, error, recipients};
action_email(GeobotID, Recipients, <<>>, Message) -> action_email(GeobotID, Recipients, undefined, Message);
action_email(GeobotID, _, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Email not sent - no subject~n", []),
    {email, error, subject};
action_email(GeobotID, Recipients, Subject, <<>>) -> action_email(GeobotID, Recipients, Subject, undefined);
action_email(GeobotID, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Email not sent - no message~n", []),
    {email, error, message};
action_email(GeobotID, Recipients, Subject, Message) ->
    RecipientsList = process_emails(Recipients),
    case imersia_smtp:send_emails(RecipientsList, Subject, Message) of
        {ok, sent} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Email sent to ~p~n", [RecipientsList]),
            {email, ok};
        {error, Reason} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Email not sent with error ~p - check mrserver email settings~n", [Reason]),
            {email, error, send}
    end.

process_emails(Recipients) when is_list(Recipients) -> Recipients;
process_emails(Recipients) when is_binary(Recipients) ->
    RecipientsDividedByCommas = binary:split(Recipients, <<",">>, [global]),
    remove_quotes_and_spaces(RecipientsDividedByCommas);
process_emails(_) -> [].

remove_quotes_and_spaces([]) -> [];
remove_quotes_and_spaces([Recipient | Recipients]) ->
    [binary:replace(Recipient, [<<"\"">>, <<"\'">>, <<" ">>], <<>>, [global]) | remove_quotes_and_spaces(Recipients)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set either a Geobot's core features (eg, title, geohash, radius) or a metadata value.  The key can be
% a JSON path, in which case the first part refers to the metadata key, and the subsequent parts to the
% JSON structure in the value.  If the JSON path does not yet exist in the value, it is constructed, and
% the resulting combined structure returned - with the given value set in place.
% ----------------------------------------------------------------------------------------------------
action_set(GeobotID, <<>>, EventParameters, Value) -> action_set(GeobotID, undefined, EventParameters, Value);
action_set(GeobotID, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error setting Variable - no variable defined~n", []),
    {set, error, key};
action_set(GeobotID, Parameter, EventParameters, <<>>) -> action_set(GeobotID, Parameter, EventParameters, undefined);
action_set(GeobotID, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error setting Variable - no value defined~n", []),
    {set, error, value};
action_set(GeobotID, <<"name">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"name">>, EventParameters, Value);
action_set(GeobotID, <<"description">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"description">>, EventParameters, Value);
action_set(GeobotID, <<"imageurl">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"imageurl">>, EventParameters, <<"/files/", GeobotID/binary, "/", Value/binary>>);
action_set(GeobotID, <<"radius">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"radius">>, EventParameters, Value);
action_set(GeobotID, <<"class">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"class">>, EventParameters, Value);
action_set(GeobotID, <<"hidden">>, EventParameters, Value) -> set_geobot_parameter(GeobotID, <<"hidden">>, EventParameters, Value);
action_set(GeobotID, <<"geohash">>, EventParameters, Value) ->
    Geobot = get_geobot(GeobotID),
    CurrentLocation = Geobot#geobot.location,
    Location = [{<<"geohash">>, Value}, {<<"altitude">>, CurrentLocation#location.altitude}],
    set_geobot_parameter(GeobotID, <<"location">>, EventParameters, Location);
action_set(GeobotID, <<"altitude">>, EventParameters, Value) ->
    Geobot = get_geobot(GeobotID),
    CurrentLocation = Geobot#geobot.location,
    Location = [{<<"geohash">>, CurrentLocation#location.geohash}, {<<"altitude">>, Value}],
    set_geobot_parameter(GeobotID, <<"location">>, EventParameters, Location);
action_set(GeobotID, <<"latitude">>, EventParameters, Value) ->
    Geobot = get_geobot(GeobotID),
    CurrentLocation = Geobot#geobot.location,
    Location = [{<<"latitude">>, Value}, {<<"longitude">>, CurrentLocation#location.longitude}, {<<"altitude">>, CurrentLocation#location.altitude}],
    set_geobot_parameter(GeobotID, <<"location">>, EventParameters, Location);
action_set(GeobotID, <<"longitude">>, EventParameters, Value) ->
    Geobot = get_geobot(GeobotID),
    CurrentLocation = Geobot#geobot.location,
    Location = [{<<"longitude">>, Value}, {<<"latitude">>, CurrentLocation#location.latitude}, {<<"altitude">>, CurrentLocation#location.altitude}],
    set_geobot_parameter(GeobotID, <<"location">>, EventParameters, Location);
action_set(GeobotID, Key, EventParameters, Value) ->
    set_geobot_metadata(GeobotID, Key, EventParameters, Value).

% Set a base value of a Geobot - not a metadata value.
set_geobot_parameter(GeobotID, Parameter, _, Value) ->
    Connection = imersia_db:connect(),
    Geobot = imersia_misc:json_to_record(geobot, [{Parameter, Value}]),
    Result = case imersia_db:geobot_setdetails(Connection, GeobotID, Geobot) of
        {ok, updated} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Parameter ~s set to value ~p~n", [Parameter, Value]),
            {set, ok};
        {error, _} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error setting Parameter - system problem~n", []),
            {set, error, database}
    end,
    imersia_db:close(Connection),
    Result.

% Set a metadata value of a geobot
set_geobot_metadata(GeobotID, Key, EventParameters, Value) ->
    {MetadataKey, JSONPath} = split_metadata_key(GeobotID, Key, EventParameters),
    ExistingJSON = case get_metadata_from_db(GeobotID, MetadataKey) of
        {ok, V} -> V;
        _ -> []
    end,

    NewJSONValue = combine_value_and_json_path(JSONPath, ExistingJSON, Value),

    Connection = imersia_db:connect(),
    Result = case imersia_db:metadata_set(Connection, GeobotID, MetadataKey, NewJSONValue) of
        {ok, _MetadataID} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Metadata ~s set with value ~p~n", [Key, Value]),
            {set, ok};
        {error, _} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error setting Metadata - system problem~n", []),
            {set, error, database}
    end,
    imersia_db:close(Connection),
    Result.

% Convert a metadata key from a colon-divided binary string to a list.
split_metadata_key (GeobotID, Variable, EventParameters) ->
    [MetadataKey | JSONPathRaw] = binary:split(Variable, <<":">>, [global]),
    % Convert the numbers from binary to numbers
    JSONPath = convert_to_bin_and_number({GeobotID, MetadataKey, EventParameters}, JSONPathRaw, err, 0),
    {MetadataKey, JSONPath}.

% Go through the JSON path and JSON structure to set the new JSON value into its correct place,
% creating new structure as needed.
combine_value_and_json_path([], _, NewJSON) -> NewJSON;
combine_value_and_json_path(Keys, [], NewJSON) -> construct_json(Keys, NewJSON);

combine_value_and_json_path([Key | Keys], ExistingValue, NewJSON) when is_binary(Key) ->
    ExistingJSON = if
        is_list(ExistingValue) -> ExistingValue;
        true -> []
    end,
    case lists:keyfind(Key, 1, ExistingJSON) of
        false ->
            ExistingJSON ++ [{Key, construct_json(Keys, NewJSON)}];
        {_, Item} ->
            NewValue = combine_value_and_json_path(Keys, Item, NewJSON),
            lists:keyreplace(Key, 1, ExistingJSON, {Key, NewValue})
    end;

combine_value_and_json_path([Index | Keys], ExistingValue, NewJSON) when is_number(Index) ->
    ExistingJSON = if
        is_list(ExistingValue) -> ExistingValue;
        true -> []
    end,
    {StartOfArray, ItemThatMatchesKey, EndOfArray} = extract_item_from_array(round(Index), ExistingJSON),
    StartOfArray ++ [combine_value_and_json_path(Keys, ItemThatMatchesKey, NewJSON)] ++ EndOfArray;

combine_value_and_json_path(_, _, Value) ->
    Value.

% Do the actual JSON structure addition.
construct_json([], NewJSON) -> NewJSON;
construct_json([Head | Tail], NewJSON) when is_binary(Head) -> [{Head, construct_json(Tail, NewJSON)}];
construct_json([Head | Tail], NewJSON) when is_integer(Head) -> [construct_json(Tail, NewJSON)].

% Extract an item from an array and return it, and the rest of the array as sub arrays in front and behind.
extract_item_from_array(1, [H | T]) ->
    {[], [H], T};
extract_item_from_array(Index, List) when Index > 1, Index =< length(List) ->
    {L1, L2} = lists:split(Index, List),
    L3 = lists:droplast(L1),
    {L3, [lists:nth(Index, List)], L2};
extract_item_from_array(Index, List) when Index > length(List) ->
    {List, [], []};
extract_item_from_array(Index, List) when Index =< 0 ->
    {[], [], List}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send an event depending on the result of the test
% ----------------------------------------------------------------------------------------------------
action_test(GeobotID, <<>>, Test, TrueEvent, FalseEvent) -> action_test(GeobotID, GeobotID, Test, TrueEvent, FalseEvent);
action_test(GeobotID, undefined, Test, TrueEvent, FalseEvent) -> action_test(GeobotID, GeobotID, Test, TrueEvent, FalseEvent);

action_test(GeobotID, DestinationID, <<>>, TrueEvent, FalseEvent) -> action_test(GeobotID, DestinationID, undefined, TrueEvent, FalseEvent);
action_test(GeobotID, _, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error testing - nothing to test~n", []),
    {test, error, test};

action_test(GeobotID, DestinationID, true, <<>>, FalseEvent) -> action_test(GeobotID, DestinationID, true, undefined, FalseEvent);
action_test(GeobotID, _, true, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "No true event~n", []),
    {test, error, true_event};

action_test(GeobotID, DestinationID, true, TrueEvent, _) ->
    DestinationIDAtom = binary_to_atom(DestinationID, utf8),
    case whereis(DestinationIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error testing - Geobot can not be found~n", []),
            {test, error, geobot};
        _ ->
            send_on_delay(GeobotID, 0, DestinationIDAtom, TrueEvent, [{}]),
            imersia_misc:debug_to_logfile(GeobotID, debug, "True event ~s sent to ~s~n", [TrueEvent, DestinationID]),
            {test, ok}
    end;

action_test(GeobotID, DestinationID, false, TrueEvent, <<>>) -> action_test(GeobotID, DestinationID, false, TrueEvent, undefined);
action_test(GeobotID, _, false, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "No false event~n", []),
    {test, error, false_event};

action_test(GeobotID, DestinationID, false, _, FalseEvent) ->
    DestinationIDAtom = binary_to_atom(DestinationID, utf8),
    case whereis(DestinationIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error testing - Geobot can not be found~n", []),
            {test, error, geobot};
        _ ->
            send_on_delay(GeobotID, 0, DestinationIDAtom, FalseEvent, [{}]),
            imersia_misc:debug_to_logfile(GeobotID, debug, "False event ~s sent to ~s~n", [FalseEvent, DestinationID]),
            {test, ok}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Process an expression (input as a binary string) by tokenizing it (using leex), then going through
% the tokens and making appropriate substitutions for variables and functions yecc can't do, then
% passing the tokens through yecc to do the grammar interpretation, hopefully ending up with a result
% that can be sent back.  This is a recursive process, with expressions inside various parts of the
% expression also being evaluated - depth first - to produce the final result.
% ----------------------------------------------------------------------------------------------------
process_expression(_, undefined, _) -> undefined;
process_expression(_, null, _) -> undefined;
process_expression(_, "", _) -> <<>>;
process_expression(_, <<>>, _) -> <<>>;
process_expression(EventData, Expression, Expecting) -> process_expression(EventData, Expression, Expecting, 0).

process_expression(_, undefined, _, _) -> undefined;
process_expression(EventData, Expression, Expecting, Depth) when is_binary(Expression) ->
    Tokens = tokenize(EventData, Expression, Depth),
    Substituted = substitute(EventData, Tokens, Expecting, Depth),
    Evaluated = evaluate(EventData, Substituted, Depth),
    if
        Evaluated == err ->
            case Expecting of
                str -> Expression;
                num -> imersia_misc:convert_number(Expression, 0);
                bool -> imersia_misc:convert_bool(Expression, false);
                _ -> err
            end;
        true -> Evaluated
    end;
process_expression(_, Expression, _, _) -> Expression.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Split an expression up into tokens according to the grammar.
% The Depth value is used to ensure we don't end up with infinite loops where variables reference
% either themselves or other variables that reference them and back.
% ----------------------------------------------------------------------------------------------------
tokenize(_, _, Depth) when Depth > 10 -> [{err, 1, err}];
tokenize(_, Expression, _) when is_binary(Expression) ->
    try
        case imersia_maths_leex:string(binary_to_list(Expression)) of
            {ok, ValueTokens, _} ->
                ValueTokens;
            _Error -> [{err, 1, err}]
        end
    catch
        _:_ -> [{err, 1, err}]
    end;
tokenize(_, Expression, _) -> [tokenize_value(Expression)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Go through the tokens and substitute each variable with an evaluated expression extracted from the
% metadata value represented by the variable.
% Note that the length function needs to be treated outside of yecc as it might be passing in an array
% (which yecc doesn't like)
% ----------------------------------------------------------------------------------------------------
substitute(_, [], _, _) -> [];
substitute(EventData, [{var, _, Variable} | Tokens], Expecting, Depth) ->
    SubstitutedVariable = value_from_json_path(EventData, Variable, Expecting, Depth),
    [SubstitutedVariable | substitute(EventData, Tokens, Expecting, Depth)];
substitute(EventData, [{func, _, <<"length">>} | Tokens], Expecting, Depth) ->
    case check_length_func(Tokens) of
        {ok, Parameter, TokensLeft} -> [{num, 1, get_length(EventData, Parameter, Depth)} | substitute(EventData, TokensLeft, Expecting, Depth)];
        {str, Parameter, TokensLeft} -> [{num, 1, string:length(Parameter)} | substitute(EventData, TokensLeft, Expecting, Depth)];
        {error} -> [{err, 1, err} | substitute(EventData, Tokens, Expecting, Depth)]
    end;
substitute(EventData, [NonVariable | Tokens], Expecting, Depth) ->
    [NonVariable | substitute(EventData, Tokens, Expecting, Depth)].

% Determine if the next set of tokens is for finding the length of an array or string
check_length_func([{lbr, 1} | [{var, 1, Parameter} | [{rbr, 1} | Tokens]]]) -> {ok, Parameter, Tokens};
check_length_func([{lbr, 1} | [{str, 1, Parameter} | [{rbr, 1} | Tokens]]]) -> {str, Parameter, Tokens};
check_length_func(_) -> error.

% Get the length of a string (ie binary) or array of objects as appropriate.
get_length(EventData, Parameter, Depth) when is_binary(Parameter) ->
    Processed_Parameter = process_expression(EventData, Parameter, err, Depth + 1),
    string:length(Processed_Parameter);
get_length(_, Parameter, _) when is_list(Parameter) -> array:size(Parameter);
get_length(_, _, _) -> 0.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Evaluate a variable by checking whether it is a json path, and if so extracting the value from the
% metadata value JSON
% ----------------------------------------------------------------------------------------------------
value_from_json_path(EventData, Variable, Expecting, Depth) when is_binary(Variable) ->
    % Split the Variable up by the colon
    [MetadataKey | JSONPathRaw] = binary:split(Variable, <<":">>, [global]),

    % Convert the numbers from binary to numbers
    JSONPath = convert_to_bin_and_number(EventData, JSONPathRaw, Expecting, Depth),

    Value = get_variable_by_key(EventData, MetadataKey, Depth),

    % Extract the value from the json path
    ReturnValue = case extract_value(JSONPath, Value) of
        undefined -> err;
        ExtractedValue ->
            % If that value is (potentially) an expression, process it
            if
                is_binary(ExtractedValue) ->
                    process_expression(EventData, ExtractedValue, Expecting, Depth + 1);
                true -> ExtractedValue
            end
    end,

    % Return it as a token format
    tokenize_value(ReturnValue);
value_from_json_path(_, Variable, _, _) -> tokenize_value(Variable).

% Given a JSON path and a JSON structure, find the value, or undefined if the path is not valid
extract_value([], Value) -> Value;
extract_value(_, []) -> undefined;
extract_value([Key | Keys], Value) when is_binary(Key), is_list(Value) ->
    case lists:keyfind(Key, 1, Value) of
        false -> undefined;
        {_, Item} -> extract_value(Keys, Item)
    end;
extract_value([Key | _], Value) when is_binary(Key) -> Value;
extract_value([Index | _], Value) when is_integer(Index), is_list(Value), Index < 1 ->
    lists:nth(1, Value);
extract_value([Index | _], Value) when is_integer(Index), is_list(Value), Index > length(Value) ->
    lists:nth(length(Value), Value);
extract_value([Index | Keys], Value) when is_integer(Index), is_list(Value) ->
    {_, [ItemThatMatchesKey], _} = extract_item_from_array(Index, Value),
    extract_value(Keys, ItemThatMatchesKey);
extract_value(_, _) -> undefined.

% Return a value with the appropriate details as a token for yecc
tokenize_value(err) ->
    {err, 1, err};
tokenize_value(Expression) when is_atom(Expression) ->
    {bool, 1, Expression};
tokenize_value(Expression) when is_number(Expression) ->
    {num, 1, Expression};
tokenize_value(Expression) when is_binary(Expression)->
    {str, 1, Expression};
tokenize_value(Expression) ->
    {var, 1, Expression}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert a list of strings and numbers represented in binary to binary strings and (non-binary) numbers
% taking into account if any of the JSON paths are expressions enclosed in square brackets.
% ----------------------------------------------------------------------------------------------------
convert_to_bin_and_number(_, [], _, _) -> [];
convert_to_bin_and_number(EventData, [Head | Tail], Expecting, Depth) ->
    % remove extraneous spaces
    String = binary:replace(Head, <<" ">>, <<>>, [global]),
    % check if it is a bracketed variable which needs converting to its value
    Result = case re:run(String, "^\[[0-9a-zA-Z_\.]+\]$") of
        nomatch ->
            % not a bracketed variable, so just use the actual value
            [imersia_misc:convert_number(String, String) | convert_to_bin_and_number(EventData, Tail, Expecting, Depth)];
        _ ->
            % is a bracketed variable, so remove the brackets, and interpret the bit in the middle
            Variable = string:trim(String, both, "\[\]"),
            NewHead = process_expression(EventData, Variable, Expecting, Depth + 1),
            [imersia_misc:convert_number(NewHead, NewHead) | convert_to_bin_and_number(EventData, Tail, Expecting, Depth)]
    end,
    Result.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the raw metadata value either from the geobot's metadada or the passed-in event parameters
% ----------------------------------------------------------------------------------------------------
get_variable_by_key ({GeobotID, EventKey, EventParameters}, MetadataKey, _Depth) ->
    try ej:get({MetadataKey}, EventParameters) of
        undefined ->
            % Use the value from the metadata as it wasn't in the event parameters
            get_metadata_by_key({GeobotID, EventKey, EventParameters}, MetadataKey);
        Value -> Value
            % Use the value from the event parameters
    catch
        _:_ ->
            get_metadata_by_key({GeobotID, EventKey, EventParameters}, MetadataKey)
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the raw metadata value from a geobot.  If it doesn't exist, create it and set it to 0.
% ----------------------------------------------------------------------------------------------------
get_metadata_by_key({GeobotID, _, _}, <<"geobotid">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.geobotid;
get_metadata_by_key({GeobotID, _, _}, <<"channelid">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.channelid;
get_metadata_by_key({GeobotID, _, _}, <<"ownerid">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.ownerid;
get_metadata_by_key({GeobotID, _, _}, <<"name">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.name;
get_metadata_by_key({GeobotID, _, _}, <<"description">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.description;
get_metadata_by_key({GeobotID, _, _}, <<"class">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.class;
get_metadata_by_key({GeobotID, _, _}, <<"imageurl">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.imageurl;
get_metadata_by_key({GeobotID, _, _}, <<"radius">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.radius;
get_metadata_by_key({GeobotID, _, _}, <<"hidden">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.hidden;
get_metadata_by_key({GeobotID, _, _}, <<"created">>) -> Geobot = get_geobot(GeobotID), convert_date_to_json(Geobot#geobot.created);
get_metadata_by_key({GeobotID, _, _}, <<"modified">>) -> Geobot = get_geobot(GeobotID), convert_date_to_json(Geobot#geobot.modified);
get_metadata_by_key({GeobotID, _, _}, <<"geohash">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.location#location.geohash;
get_metadata_by_key({GeobotID, _, _}, <<"altitude">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.location#location.altitude;
get_metadata_by_key({GeobotID, _, _}, <<"latitude">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.location#location.latitude;
get_metadata_by_key({GeobotID, _, _}, <<"longitude">>) -> Geobot = get_geobot(GeobotID), Geobot#geobot.location#location.longitude;
get_metadata_by_key({GeobotID, _, _}, MetadataKeyIn) ->
    [MetadataKey | Rest] = string:split(MetadataKeyIn, <<"!">>),

    case get_metadata_from_db(GeobotID, MetadataKey) of
        {ok, Metadata} -> Metadata;
        {error, _} ->
            case Rest of
                [<<>>] -> % Variable ends in ! so initialise as new metadata if it doesn't exist
                    Connection = imersia_db:connect(),
                    Result = case imersia_db:metadata_set(Connection, GeobotID, MetadataKey, 0) of
                        {ok, _MetadataID} -> 0;
                        {error, _} -> err
                    end,
                    imersia_db:close(Connection),
                    Result;
                [] -> % No ! at end of variable name, so if it doesn't exist, just return a string of the key
                    <<"'", MetadataKey/binary, "'">>
            end
    end.

get_metadata_from_db(GeobotID, MetadataKey) ->
    Connection = imersia_db:connect(),
    MetadataValue = case imersia_db:metadata_get(Connection, GeobotID, MetadataKey) of
        {ok, Metadata} ->
            {ok, Metadata#metadata.value};
        Error -> Error
    end,
    imersia_db:close(Connection),
    MetadataValue.

convert_date_to_json({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
    [{<<"year">>, Year}, {<<"month">>, Month}, {<<"day">>, Day}, {<<"hours">>, Hours}, {<<"minutes">>, Minutes}, {<<"seconds">>, Seconds}];
convert_date_to_json(_) -> [].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Take a sequence of tokens, and evaluate according to the grammar
% ----------------------------------------------------------------------------------------------------
evaluate(_, Tokens, _) ->
    try
        % make sure there is an end - doesn't matter if there are two end tokens.
        ParsedTokens = imersia_maths_yecc:parse(Tokens ++ [{'$end'}]),
        case ParsedTokens of
            {ok, {num, _, Answer}} -> Answer;
            {ok, {bool, _, Answer}} -> Answer;
            {ok, {var, _, Answer}} -> Answer;
            {ok, {str, _, Answer}} -> Answer;
            _ -> err
        end
    catch
        _:_ -> err
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the current Geobot details.
% ----------------------------------------------------------------------------------------------------
get_geobot(GeobotID) ->
    Connection = imersia_db:connect(),
    {ok, Geobot} = imersia_db:geobot_getdetails(Connection, GeobotID),
    imersia_db:close(Connection),
    Geobot.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send an event with parameters to another Geobot
% ----------------------------------------------------------------------------------------------------
action_send(GeobotID, <<>>, DestinationID, Event, Delay, Parameters) -> action_send(GeobotID, undefined, DestinationID, Event, Delay, Parameters);
action_send(GeobotID, URL, <<>>, Event, Delay, Parameters) -> action_send(GeobotID, URL, GeobotID, Event, Delay, Parameters);
action_send(GeobotID, URL, undefined, Event, Delay, Parameters) -> action_send(GeobotID, URL, GeobotID, Event, Delay, Parameters);
action_send(GeobotID, URL, DestinationID, Event, Delay, undefined) -> action_send(GeobotID, URL, DestinationID, Event, Delay, []);
action_send(GeobotID, URL, DestinationID, Event, undefined, Parameters) -> action_send(GeobotID, URL, DestinationID, Event, 0, Parameters);

action_send(GeobotID, URL, DestinationID, <<>>, Delay, Parameters) -> action_send(GeobotID, URL, DestinationID, undefined, Delay, Parameters);
action_send(GeobotID, _, _, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error sending - no event specified~n", []),
    {send, error, event};

action_send(GeobotID, undefined, DestinationID, Event, Delay, Parameters) ->
    DestinationIDAtom = binary_to_atom(DestinationID, utf8),
    DelayMilliseconds = round(check_number(Delay) * 1000),
    case whereis(DestinationIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error sending - Geobot can not be found~n", []),
            {send, error, geobotid};
        _ ->
            send_on_delay(GeobotID, DelayMilliseconds, DestinationIDAtom, Event, Parameters),
            imersia_misc:debug_to_logfile(GeobotID, debug, "Event ~s sent to ~s with parameters ~p~n", [Event, DestinationIDAtom, Parameters]),
            {send, ok}
    end;
action_send(GeobotID, URL, DestinationID, Event, Delay, Parameters) ->
    EntanglementProcessAtom = binary_to_atom(<< DestinationID/binary, "@", URL/binary >>, utf8),
    DelayMilliseconds = round(check_number(Delay) * 1000),
    case whereis(EntanglementProcessAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error sending - Entanglement process can not be found~n", []),
            {send, error, geobotid};
        _ ->
            send_on_delay(GeobotID, DelayMilliseconds, EntanglementProcessAtom, Event, Parameters),
            imersia_misc:debug_to_logfile(GeobotID, debug, "Event ~s sent to ~s with parameters ~p~n", [Event, EntanglementProcessAtom, Parameters]),
            {send, ok}
    end.

% Ensure it is a valid number, or zero
check_number(X) when is_number(X) -> X;
check_number(_) -> 0.

% Send an event after a specified delay.
% send_on_delay(GeobotID, 0, DestinationIDAtom, Event, Parameters) -> send_on_delay(GeobotID, 50, DestinationIDAtom, Event, Parameters);
send_on_delay(GeobotID, DelayMilliseconds, DestinationIDAtom, Event, Parameters) ->
    GeobotIDTimersAtom = binary_to_atom(<<GeobotID/binary, "_timers">>, utf8),
    case (timer:send_after(DelayMilliseconds, DestinationIDAtom, {event, {Event, Parameters}})) of
        {ok, TimerRef} ->
            case application:get_env(mrserver, GeobotIDTimersAtom) of
                {ok, TimerRefs} ->
                    application:set_env(mrserver, GeobotIDTimersAtom, set_this_timer(TimerRefs, Event, TimerRef));
                _ ->
                    application:set_env(mrserver, GeobotIDTimersAtom, [{Event, TimerRef}])
            end;
        {error, _Error} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error setting timer for ~s~n", [GeobotIDTimersAtom])
    end.

% Ensure that only one timer for any specific event is kept at any one time.  So, a new event will supercede
% an event that was sent earlier that is still waiting to fire.
set_this_timer([], Event, TimerRef) -> [{Event, TimerRef}];
set_this_timer([{Event, OldTimerRef} | TimerRefs], Event, NewTimerRef) ->
    timer:cancel(OldTimerRef),
    [{Event, NewTimerRef} | TimerRefs];
set_this_timer([TimerRef | TimerRefs], Event, NewTimerRef) ->
    [TimerRef | set_this_timer(TimerRefs, Event, NewTimerRef)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Broadcast an event to Geobots within a certain radius on a specified Channel (with delay)
% ----------------------------------------------------------------------------------------------------
action_broadcast(GeobotID, ChannelIDin, Radius, <<>>, Delay, Parameters) -> action_broadcast(GeobotID, ChannelIDin, Radius, undefined, Delay, Parameters);
action_broadcast(GeobotID, _, _, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error broadcasting - no event specified~n", []),
    {broadcast, error, event};
action_broadcast(GeobotID, ChannelIDin, <<>>, Event, Delay, Parameters) -> action_broadcast(GeobotID, ChannelIDin, 2000, Event, Delay, Parameters);
action_broadcast(GeobotID, ChannelIDin, undefined, Event, Delay, Parameters) -> action_broadcast(GeobotID, ChannelIDin, 2000, Event, Delay, Parameters);

action_broadcast(GeobotID, ChannelIDin, Radius, Event, Delay, Parameters) ->
    Geobot = get_geobot(GeobotID),
    Location = Geobot#geobot.location,
    UserID = Geobot#geobot.ownerid,
    % If no channelID is specified, use the geobots own channel
    ChannelID = choose_one(ChannelIDin, Geobot#geobot.channelid),
    % Find all the Geobots in this channel within the given Radius
    Connection = imersia_db:connect(),
    % Send the event to each of them
    Result = case imersia_db:geobot_list(Connection, ChannelID, UserID, Location, Radius, false) of
        {ok, GeobotList} ->
            GeobotsWithoutMe = remove_self_from_geobotlist(Geobot#geobot.geobotid, GeobotList),
            send_to_each_geobot(GeobotsWithoutMe, Event, Delay, Parameters),
            imersia_misc:debug_to_logfile(GeobotID, debug, "Event ~s broadcast to ~p with parameters ~p~n", [Event, geobot_ids(GeobotsWithoutMe), Parameters]),
            {broadcast, ok};
        _ ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error broadcasting - system problem~n", []),
            {broadcast, error, database}
    end,
    imersia_db:close(Connection),
    Result.

% Select either the given ChannelID, or the Geobot's own Channel
choose_one(<<"">>, B) -> B;
choose_one(undefined, B) -> B;
choose_one(A, _) -> A.

% Remove the given Geobot from the list
remove_self_from_geobotlist(_, []) -> [];
remove_self_from_geobotlist(GeobotID, [Geobot | Geobots]) when Geobot#geobot.geobotid == GeobotID -> Geobots;
remove_self_from_geobotlist(GeobotID, [Geobot | Geobots]) -> [Geobot | remove_self_from_geobotlist(GeobotID, Geobots)].

% Send an event to each Geobot on the list
send_to_each_geobot([], _, _, _) -> {broadcast, ok};
send_to_each_geobot([Geobot | Geobots], Event, Delay, Parameters) ->
    DestinationID = Geobot#geobot.geobotid,
    DestinationIDAtom = binary_to_atom(DestinationID, utf8),
    DelayMilliseconds = round(check_number(Delay) * 1000),
    case whereis(DestinationIDAtom) of
        undefined -> {broadcast, error, geobotid};
        _ ->
            send_on_delay(DestinationID, DelayMilliseconds, DestinationIDAtom, Event, Parameters),
            send_to_each_geobot(Geobots, Event, Delay, Parameters)
    end.

geobot_ids([]) -> [];
geobot_ids([Geobot | Geobots]) ->
    [Geobot#geobot.geobotid | geobot_ids(Geobots)].
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send a Trigger event to the Geobot and hence on to the Companion and out to the devices.
% ----------------------------------------------------------------------------------------------------
action_trigger(GeobotID, <<>>, Parameters) -> action_trigger(GeobotID, undefined, Parameters);
action_trigger(GeobotID, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error sending trigger - no event specified~n", []),
    {trigger, error, event};
action_trigger(GeobotID, Event, Parameters) ->
    GeobotIDIDAtom = binary_to_atom(GeobotID, utf8),
    case whereis(GeobotIDIDAtom) of
        undefined ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Error sending trigger - Geobot can not be found~n", []),
            {trigger, error, geobotid};
        _ ->
            GeobotIDIDAtom ! {trigger, Event, Parameters},
            imersia_misc:debug_to_logfile(GeobotID, debug, "Trigger ~s sent with parameters ~p~n", [Event, Parameters]),
            {trigger, ok}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Entangles with a Geobot on another MRServer.  If a Trigger occurs on the remote Geobot, this becomes an
% event on this Geobot - which can then in turn be processed to cause a local trigger, or other action.
% ----------------------------------------------------------------------------------------------------
action_entangle(GeobotID, <<>>, DeveloperID, UserEmail, Password, RemoteGeobotID) -> action_entangle(GeobotID, undefined, DeveloperID, UserEmail, Password, RemoteGeobotID);
action_entangle(GeobotID, undefined, _, _, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with entanglement - no remote server URL~n", []),
    {entangle, error, url};
action_entangle(GeobotID, MRServer, <<>>, UserEmail, Password, RemoteGeobotID) -> action_entangle(GeobotID, MRServer, undefined, UserEmail, Password, RemoteGeobotID);
action_entangle(GeobotID, _, undefined, _, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with entanglement - no developerid~n", []),
    {entangle, error, developerid};
action_entangle(GeobotID, MRServer, DeveloperID, <<>>, Password, RemoteGeobotID) -> action_entangle(GeobotID, MRServer, DeveloperID, undefined, Password, RemoteGeobotID);
action_entangle(GeobotID, _, _, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with entanglement - no useremail~n", []),
    {entangle, error, useremail};
action_entangle(GeobotID, MRServer, DeveloperID, UserEmail, <<>>, RemoteGeobotID) -> action_entangle(GeobotID, MRServer, DeveloperID, UserEmail, undefined, RemoteGeobotID);
action_entangle(GeobotID, _, _, _, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with entanglement - no password~n", []),
    {entangle, error, password};
action_entangle(GeobotID, MRServer, DeveloperID, UserEmail, Password, <<>>) -> action_entangle(GeobotID, MRServer, DeveloperID, UserEmail, Password, undefined);
action_entangle(GeobotID, _, _, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with entanglement - no remote GeobotID~n", []),
    {entangle, error, geobotid};
action_entangle(GeobotID, MRServer, DeveloperID, UserEmail, Password, RemoteGeobotID) ->
    Geobot = get_geobot(GeobotID),
    Geohash = Geobot#geobot.location#location.geohash,

    Parameters = #{
        geobotid => GeobotID,
        mrserver => MRServer,
        developerid => DeveloperID,
        useremail => UserEmail,
        password => Password,
        geohash => Geohash,
        remotegeobotid => RemoteGeobotID},

    case imersia_entanglements_sup:start_entanglement(Parameters) of
        {ok, _} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Geobot entangled with ~s@~s~n", [RemoteGeobotID, MRServer]),
            {entangle, ok};
        {error, _} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Geobot not entangled with ~s@~s~n", [RemoteGeobotID, MRServer]),
            {entangle, error, start}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Remove the entanglement between two previously entangled Geobots
% ----------------------------------------------------------------------------------------------------
action_disentangle(GeobotID, <<>>, DeveloperID, RemoteGeobotID) -> action_disentangle(GeobotID, undefined, DeveloperID, RemoteGeobotID);
action_disentangle(GeobotID, undefined, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with disentanglement - no remote server URL~n", []),
    {entangle, error, url};
action_disentangle(GeobotID, MRServer, <<>>, RemoteGeobotID) -> action_disentangle(GeobotID, MRServer, undefined, RemoteGeobotID);
action_disentangle(GeobotID, _, undefined, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with disentanglement - no developerid~n", []),
    {entangle, error, developerid};
action_disentangle(GeobotID, MRServer, DeveloperID, <<>>) -> action_disentangle(GeobotID, MRServer, DeveloperID, undefined);
action_disentangle(GeobotID, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error with disentanglement - no remote GeobotID~n", []),
    {entangle, error, geobotid};
action_disentangle(GeobotID, MRServer, DeveloperID, RemoteGeobotID) ->
    Geobot = get_geobot(GeobotID),
    Geohash = Geobot#geobot.location#location.geohash,

    Parameters = #{
        geobotid => GeobotID,
        mrserver => MRServer,
        developerid => DeveloperID,
        geohash => Geohash,
        remotegeobotid => RemoteGeobotID},

    case imersia_entanglements_sup:stop_entanglement(Parameters) of
        {ok, _} ->
            imersia_misc:debug_to_logfile(GeobotID, debug, "Geobot disentangled with ~s@~s~n", [RemoteGeobotID, MRServer]),
            {disentangle, ok};
        {error, _} ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Geobot not disentangled with ~s@~s~n", [RemoteGeobotID, MRServer]),
            {disentangle, error, start}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Load an automation from a metadata field with the key defined in Name
% ----------------------------------------------------------------------------------------------------
action_load(GeobotID, UserID, undefined, _, ContextIDs, Name) -> action_load(GeobotID, UserID, GeobotID, geobot, ContextIDs, Name);
action_load(GeobotID, UserID, <<>>, _, ContextIDs, Name) -> action_load(GeobotID, UserID, GeobotID, geobot, ContextIDs, Name);
action_load(GeobotID, UserID, EntityID, EntityType, ContextIDs, <<>>) -> action_load(GeobotID, UserID, EntityID, EntityType, ContextIDs, undefined);
action_load(GeobotID, _, _, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error loading Automation - no name~n", []),
    {load, error, name};
action_load(GeobotID, undefined, _, _, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error loading Automation - no access~n", []),
    {load, error, userid};

action_load(GeobotID, UserID, EntityID, EntityType, ContextIDs, Name) ->
    Connection = imersia_db:connect(),
    Result = case check_read_access(Connection, UserID, EntityID, ContextIDs, EntityType) of
        ok ->
            case get_metadata_from_db(EntityID, Name) of
                {ok, AutomationMap} ->
                    AutomationRaw = imersia_misc:safely_convert_from_map(AutomationMap),
                    case imersia_misc:json_to_record(automation, AutomationRaw) of
                        error ->
                            imersia_misc:debug_to_logfile(GeobotID, error, "Error interpreting Automation ~s from Metadata~n", [Name]),
                            {load, error, load_automation};
                        Automation ->
                            RenamedAutomation = Automation#automation{name = Name},
                            case imersia_db:automation_id(Connection, GeobotID, Name) of
                                {ok, AutomationID} ->
                                    case imersia_db:automation_setdetails(Connection, GeobotID, AutomationID, RenamedAutomation) of
                                        {ok, _} ->
                                            imersia_misc:debug_to_logfile(GeobotID, debug, "Automation ~s loaded from Metadata~n", [Name]),
                                            {load, ok};
                                        {error, Reason} ->
                                            imersia_misc:debug_to_logfile(GeobotID, error, "Error loading Automation ~s from Metadata - ~p~n", [Name, Reason]),
                                            {load, error, write_automation}
                                    end;
                                _ ->
                                    case imersia_db:automation_new(Connection, GeobotID, RenamedAutomation) of
                                        {ok, _} ->
                                            imersia_misc:debug_to_logfile(GeobotID, debug, "Automation ~s created from Metadata~n", [Name]),
                                            {load, ok};
                                        {error, Reason} ->
                                            imersia_misc:debug_to_logfile(GeobotID, error, "Error creating Automation ~s from Metadata - ~p~n", [Name, Reason]),
                                            {load, error, new_automation}
                                    end
                            end
                    end;
                _ ->
                    imersia_misc:debug_to_logfile(GeobotID, error, "No Automation ~s to load from Metadata~n", [Name]),
                    {load, error, metadata}
            end;
        no_access ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Access to Entity holding Automation not allowed~n", []),
            {load, error, access}
    end,
    imersia_db:close(Connection),
    Result.

check_read_access(Connection, UserID, EntityID, ContextIDs, geobot) ->
    case imersia_auth:check_geobot_attribute(Connection, get, EntityID, UserID, ContextIDs, <<"read">>) of
        {ok, _} -> ok;
        _ -> no_access
    end;
check_read_access(Connection, UserID, EntityID, ContextIDs, channel) ->
    case imersia_auth:check_channel_attribute(Connection, get, EntityID, UserID, ContextIDs, <<"read">>) of
        {ok, _} -> ok;
        _ -> no_access
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
action_save(GeobotID, UserID, undefined, EntityType, ContextIDs, Name) -> action_save(GeobotID, UserID, GeobotID, EntityType, ContextIDs, Name);
action_save(GeobotID, UserID, <<>>, EntityType, ContextIDs, Name) -> action_save(GeobotID, UserID, GeobotID, EntityType, ContextIDs, Name);
action_save(GeobotID, UserID, EntityID, EntityType, ContextIDs, <<>>) -> action_save(GeobotID, UserID, EntityID, EntityType, ContextIDs, undefined);
action_save(GeobotID, _, _, _, _, undefined) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error saving Automation - no name~n", []),
    {save, error, name};
action_save(GeobotID, undefined, _, _, _, _) ->
    imersia_misc:debug_to_logfile(GeobotID, error, "Error saving Automation - no access~n", []),
    {save, error, userid};

action_save(GeobotID, UserID, EntityID, EntityType, ContextIDs, Name) ->
    Connection = imersia_db:connect(),
    Result = case check_write_access(Connection, UserID, EntityID, ContextIDs, EntityType) of
        ok ->
            case imersia_db:automation_id(Connection, GeobotID, Name) of
                {ok, AutomationID} ->
                    case imersia_db:automation_getdetails(Connection, GeobotID, AutomationID) of
                        {ok, Automation} ->
                            AutomationJSONwithID = imersia_misc:record_to_json(Automation, true),
                            AutomationJSON = lists:keydelete(automationid, 1, AutomationJSONwithID),
                            case imersia_db:metadata_set(Connection, EntityID, Name, AutomationJSON) of
                                {ok, _} ->
                                    imersia_misc:debug_to_logfile(GeobotID, debug, "Automation ~s saved to Metadata~n", [Name]),
                                    {save, ok};
                                {error, Reason} ->
                                    imersia_misc:debug_to_logfile(GeobotID, error, "Error saving Automation ~s - ~p~n", [Name, Reason]),
                                    {save, error, system}
                            end;
                        _ ->
                            imersia_misc:debug_to_logfile(GeobotID, error, "No Automation with name ~s to save~n", [Name]),
                            {save, error, name}
                    end;
                _ ->
                    imersia_misc:debug_to_logfile(GeobotID, error, "Error saving Automation ~s~n", [Name]),
                    {save, error, name}
            end;
        no_access ->
            imersia_misc:debug_to_logfile(GeobotID, error, "Access to Entity holding Automation not allowed~n", []),
            {load, error, access}
    end,
    imersia_db:close(Connection),
    Result.

check_write_access(Connection, UserID, EntityID, ContextIDs, geobot) ->
    case imersia_auth:check_geobot_attribute(Connection, put, EntityID, UserID, ContextIDs, <<"edit">>) of
        {ok, _} -> ok;
        _ -> no_access
    end;
check_write_access(Connection, UserID, EntityID, ContextIDs, channel) ->
    case imersia_auth:check_channel_attribute(Connection, put, EntityID, UserID, ContextIDs, <<"edit">>) of
        {ok, _} -> ok;
        _ -> no_access
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Clear all the timers for a Geobot
% ----------------------------------------------------------------------------------------------------
clear_timers(GeobotID) ->
    GeobotIDTimersAtom = binary_to_atom(<<GeobotID/binary, "_timers">>, utf8),
    case application:get_env(mrserver, GeobotIDTimersAtom) of
        {ok, TimerRefs} ->
            clear_all_timers(TimerRefs),
            application:set_env(mrserver, GeobotIDTimersAtom, []);
        _ -> ok
    end.

clear_all_timers([]) -> ok;
clear_all_timers([{_Event, TimerRef} | TimerRefs]) ->
    timer:cancel(TimerRef),
    clear_all_timers(TimerRefs).
% ----------------------------------------------------------------------------------------------------
