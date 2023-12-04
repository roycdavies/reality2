% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Settings Implementation, reading from an mrserver.toml or merserver.ini file in the main directory
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_settings).

-export([init/0, developerid/0, get_version/0, get_setting/2]).

% ----------------------------------------------------------------------------------------------------
% Read the INI file and set the various settings.  This has to be called very early on.
% ----------------------------------------------------------------------------------------------------
init() ->
    case application:get_env(mrserver, initialise) of
        undefined ->
            application:set_env(mrserver, initialise, true),
            INIfile = imersia_misc:basedir() ++ "/mrserver.ini",
            TOMLfile = imersia_misc:basedir() ++ "/mrserver.toml",

            % Try the TOML file first, then if that doesn't exist, look for an INI file.
            FileData = case file:read_file(TOMLfile) of
                {ok, TOMLData} ->
                    io:format("Loading settings from ~s.~n", [TOMLfile]),
                    {toml, TOMLData};
                _ ->
                    case file:read_file(INIfile) of
                        {ok, INIData} ->
                            io:format("Loading settings from ~s.~n", [INIfile]),
                            {ini, INIData};
                        _ ->
                            io:format("Settings file not found.~n", []),
                            undefined
                    end
            end,

            set_settings(FileData),

            case imersia_settings:get_setting(mrserver, debug) of
                <<"true">> -> io:format("DEBUGGING IS ON~n", []);
                _ -> io:format("DEBUGGING IS OFF~n", [])
            end,

            case get_setting(database, implementation) of
                <<"mnesia">> ->
                    application:stop(mnesia),
                    case get_setting(mrserver, name) of
                        {ok, ServerName} ->
                            mnesia:create_schema([binary_to_atom(ServerName, utf8)]);
                        _ -> mnesia:create_schema([mrserver@mrserver])
                    end,
                    application:start(mnesia);
                _ -> ok
            end;
        _ -> ok
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the developerID
% ----------------------------------------------------------------------------------------------------
developerid() ->
    case application:get_env(mrserver, developerid) of
        {ok, Value} -> Value;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Interpret the settings from the INI file and set the various values
% ----------------------------------------------------------------------------------------------------
% Default values if file cannot be found or is invalid
set_settings(undefined) ->
    imersia_misc:debug(error, "mrserver.toml file is either not found or unreadable - using default values.~n", []),
    application:set_env(mrserver, database_implementation, mnesia, [{persistent, true}]),
    application:set_env(mrserver, file_implementation, local, [{persistent, true}]),
    application:set_env(mnesia, dir, "/MRServer/database", [{persistent, true}]),
    application:set_env(mrserver, name, "mrserver@mrserver", [{persistent, true}]),
    ok;

% Values from the file
set_settings({toml, FileData}) ->
    case etoml:parse(FileData) of
        {ok, Settings} -> parse_settings(Settings);
        _ -> set_settings(undefined)
    end,
    ok;
set_settings({ini, FileData}) ->
    case zucchini:parse_string(FileData) of
        {ok, Settings} -> parse_settings(Settings);
        _ -> set_settings(undefined)
    end,
    ok.

parse_settings([]) -> ok;
parse_settings([Setting | Settings]) ->
    {Category, MinorSettings} = Setting,
    parse_minor_settings(Category, MinorSettings),
    parse_settings(Settings).

parse_minor_settings(_, []) -> ok;
parse_minor_settings(<<"mnesia">>, [MinorSetting | MinorSettings]) ->
    {Key, Value} = MinorSetting,
    application:set_env(mnesia, ensure_atom(Key), ensure_list(Value), [{persistent, true}]),
    parse_minor_settings(<<"mnesia">>, MinorSettings);
parse_minor_settings(mnesia, [MinorSetting | MinorSettings]) ->
    {Key, Value} = MinorSetting,
    application:set_env(mnesia, ensure_atom(Key), ensure_list(Value), [{persistent, true}]),
    parse_minor_settings(mnesia, MinorSettings);
parse_minor_settings(CategoryBin, [MinorSetting | MinorSettings]) when is_binary(CategoryBin) ->
    {KeyBin, Value} = MinorSetting,
    Varname = binary_to_atom(<<CategoryBin/binary, "_", KeyBin/binary>>, utf8),
    application:set_env(mrserver, Varname, Value, [{persistent, true}]),
    parse_minor_settings(CategoryBin, MinorSettings);
parse_minor_settings(Category, [MinorSetting | MinorSettings]) when is_atom(Category) ->
    {Key, Value} = MinorSetting,
    CategoryBin = atom_to_binary(Category, utf8),
    KeyBin = atom_to_binary(Key, utf8),
    Varname = binary_to_atom(<<CategoryBin/binary, "_", KeyBin/binary>>, utf8),
    application:set_env(mrserver, Varname, Value, [{persistent, true}]),
    parse_minor_settings(Category, MinorSettings).

ensure_atom(A) when is_binary(A) -> binary_to_atom(A, utf8);
ensure_atom(A) when is_list(A) -> list_to_atom(A);
ensure_atom(A) -> A.

ensure_list(A) when is_binary(A) -> binary_to_list(A);
ensure_list(A) when is_atom(A) -> atom_to_list(A);
ensure_list(A) -> A.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return the given setting
% ----------------------------------------------------------------------------------------------------
get_version() ->
    get_setting(mrserver, version).

get_setting(mrserver, version) ->
    case application:get_env(mrserver, version) of
        {ok, Val} when is_atom(Val) -> erlang:atom_to_binary(Val, utf8);
        {ok, Val} when is_list(Val) -> erlang:list_to_binary(Val);
        {ok, Val} when is_binary(Val) -> Val;
        _ -> undefined
    end;

get_setting(mrserver, admin_exception) ->
        case application:get_env(mrserver, admin_exception) of
        {ok, Val} when is_atom(Val) -> erlang:atom_to_binary(Val, utf8);
        {ok, Val} when is_list(Val) -> erlang:list_to_binary(Val);
        {ok, Val} when is_binary(Val) -> Val;
        _ -> undefined
    end;

get_setting(Category, Key) ->
    case application:get_env(mrserver, initialise) of
        undefined -> init(), do_get_setting(Category, Key);
        _ -> do_get_setting(Category, Key)
    end.

do_get_setting(Category, Key) ->
    CategoryBin = atom_to_binary(Category, utf8),
    KeyBin = atom_to_binary(Key, utf8),
    Varname = binary_to_atom(<<CategoryBin/binary, "_", KeyBin/binary>>, utf8),
    case application:get_env(mrserver, Varname) of
        {ok, Val} when is_atom(Val) -> erlang:atom_to_binary(Val, utf8);
        {ok, Val} when is_list(Val) -> erlang:list_to_binary(Val);
        {ok, Val} when is_binary(Val) -> Val;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------
