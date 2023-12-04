%% @author atill
%%
%% @doc
%% CSV parsing module.  Parsed CSV will be processed and converted
%% into a list containing the separated values, lines are separated
%% by the newline atom.

%% Note that some pre-processing of the CSV file is required as this
%% code doesn't deal with quotes inside quoted sections of the file,
%% which are usually written as two quotes together, eg "He said ""hello""."
%% Best way is to change all doubled quotes to one single quote.

-module(imersia_csv).

-import(lists, [reverse/1]).

-export([parse_from_io/1, parse_from_string/1, iterate_chars/4]).

-export([parser_process/0]).

%% @doc parse using an io device such as file as the string source
parse_from_io(IoDevice) ->
    IoDeviceIterator = fun(Io) ->
        {io:get_chars(Io, "", 1), Io}
    end,
    iterate_chars(spawn(?MODULE, parser_process, []), IoDeviceIterator, IoDevice).

%% @doc parse csv from a string
parse_from_string(String) ->
    StringIterator = fun(StringList) ->
        get_first_char(StringList)
    end,
    iterate_chars(spawn(?MODULE, parser_process, []), StringIterator, String).

%% @doc function used internally for the parser process, do NOT use!
parser_process() ->
    ready().

%%
%% Local Functions
%%

iterate_chars(ParserPid, IteratorFun, TextSource) ->
    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(ParserPid, IteratorFun, UpdatedTextSource, FirstChar).

iterate_chars(Pid, _, _, eof) ->
    Pid ! {eof, self()},
    receive
        {ParsedCsv} ->
            ParsedCsv
    end;

iterate_chars(Pid, IteratorFun, TextSource, Char) ->
    Pid ! {clean_char_argument(Char)},

    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(Pid, IteratorFun, UpdatedTextSource, FirstChar).

%% @doc make sure that an integer denoting a char is returned instead of a string
clean_char_argument([CharInt | _]) ->
    CharInt;
clean_char_argument(CharInt) when is_integer(CharInt) ->
    CharInt.

%% @doc returns tuple {FirstChar, RemainingChars} or {eof, []} if no more chars
%% remains
get_first_char([]) ->
    {eof, []};
get_first_char([FirstChar | Tail]) ->
    {FirstChar, Tail}.

%%
%% CSV State Machine
%%

-define(EMPTY_STRING, []).

-define(CSV_EOF_PATTERN, {eof, ResultPid}).
-define(CSV_EOF,
    CsvLine = reverse([reverse(CurrentValue) | ParsedCsv]),
    ResultPid ! {CsvLine}
).

% the ready state awaits chars to be passed to it and builds the a string
% between the value delimiter.
% if a quote is encountered then the in_quotes state is moved to.
ready() ->
    ready([], []).
ready(ParsedCsv, CurrentValue) ->
    receive
        {Char} when (Char == $") -> %or (Char == $') ->
            % pass an empty string to in_quotes as we do not want the
            % preceeding characters to be included, only those in quotes
            in_quotes(ParsedCsv, ?EMPTY_STRING, Char);
        {Char} when Char == $, ->
            ready([reverse(CurrentValue) | ParsedCsv], ?EMPTY_STRING);
        {Char} when Char == $\n ->
            % insert a newline atom when a newline char is received
            List = [newline | [reverse(CurrentValue) | ParsedCsv]],
            ready(List, ?EMPTY_STRING);
        {Char} when Char == $\r ->
            % ignore line feed characters
            ready(ParsedCsv, CurrentValue);
        {Char} ->
            ready(ParsedCsv, [Char | CurrentValue]);
        ?CSV_EOF_PATTERN ->
            ?CSV_EOF
    end.

% the in_quotes state adds all chars it receives to the value string until
% it receives a char matching the initial quote in which case it moves to
% the skip_to_delimiter state.
in_quotes(ParsedCsv, CurrentValue, QuoteChar) ->
    receive
        {Char} when Char == QuoteChar ->
            skip_to_delimiter([reverse(CurrentValue) | ParsedCsv]);
        {Char} ->
            in_quotes(ParsedCsv, [Char | CurrentValue], QuoteChar);
        ?CSV_EOF_PATTERN ->
            ?CSV_EOF
    end.

% the skip_to_delimiter awaits chars which will get thrown away, when a
% value delimiter is received the machine moves to the ready state again.
skip_to_delimiter(ParsedCsv) ->
    receive
        {Char} when Char == $, ->
            ready(ParsedCsv, ?EMPTY_STRING);
        ?CSV_EOF_PATTERN ->
            % we are not building a value with the chars we receive so just
            % pass the already parsed list
            ResultPid ! {reverse(ParsedCsv)};
        {_} ->
            skip_to_delimiter(ParsedCsv)
    end.
