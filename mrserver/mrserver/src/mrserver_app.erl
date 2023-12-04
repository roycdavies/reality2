% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The core MR Server App
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(mrserver_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% ----------------------------------------------------------------------------------------------------
%% @doc Start and stop Cowboy web server
%% ----------------------------------------------------------------------------------------------------
start(_Type, _Args) ->
    imersia_settings:init(),

    %observer:start(),

    HTTPFolder = case imersia_settings:get_setting(mrserver, httpfolder) of
        undefined -> "http";
        V1 -> erlang:binary_to_list(V1)
    end,

    HTTPSFolder = case imersia_settings:get_setting(mrserver, httpsfolder) of
        undefined -> "https";
        V2 -> erlang:binary_to_list(V2)
    end,

    HTTPSDefaultFolder = case imersia_settings:get_setting(mrserver, httpsdefault) of
        undefined -> "";
        V3 -> erlang:binary_to_list(V3)
    end,

    HTTPDefaultFolder = case imersia_settings:get_setting(mrserver, httpdefault) of
        undefined -> "";
        V4 -> erlang:binary_to_list(V4)
    end,

    HTTPSSubFolders = case file:list_dir_all(imersia_misc:basedir() ++ "/" ++ HTTPSFolder) of
        {ok, Files} -> Files;
        _ -> []
    end,
    AdditionalHTTPSFolders = process_folders_list(imersia_misc:basedir(), HTTPSFolder, HTTPSSubFolders),
    DefaultHTTPSFolder = process_default_folder(imersia_misc:basedir(), HTTPSFolder, HTTPSDefaultFolder),

    HTTPSubFolders = case file:list_dir_all(imersia_misc:basedir() ++ "/" ++ HTTPFolder) of
        {ok, Files2} -> Files2;
        _ -> []
    end,
    AdditionalHTTPFolders = process_folders_list(imersia_misc:basedir(), HTTPFolder, HTTPSubFolders),
    DefaultHTTPFolder = process_default_folder(imersia_misc:basedir(), HTTPFolder, HTTPDefaultFolder),

    Dispatch = cowboy_router:compile([
        {'_', [

            {"/api/user/password[/:userid]", imersia_user_password_api, []},
            {"/api/user/tokens[/:userid]", imersia_user_tokens_api, []},
            {"/api/user[/:userid]", imersia_user_api, []},
            % {"/api/companion", imersia_companion_api, []},
            {"/api/sessions[/:sessionid]", imersia_sessions_api, []},

            % Channels and Geobots API
            {"/api/channels[/:name]", imersia_channels_api, []},
            {"/api/geobots", imersia_geobots_api, []},

            % Log an event to a Geobot
            {"/api/geobots/log", imersia_geobots_log_api, []},

            % Send an event to a Geobots automations
            {"/api/geobots/send", imersia_geobots_send_api, []},

            % Automations API
            {"/api/geobots/automations", imersia_automations_api, []},

            % Metadata API
            {"/api/metadata", imersia_metadata_api, []},

            % Passcodes API
            {"/api/passcodes", imersia_passcodes_api, []},

            % Context API
            {"/api/context", imersia_context_api, []},

            % Analytics api
            {"/api/analytics", imersia_analytics_api, []},

            % Vars api
            {"/api/vars", imersia_vars_api, []},

            % Admin api
            {"/api/admin", imersia_admin_api, []},

            % Files API
            {"/api/files", imersia_files_api, []},

            % Files server (content)
            {"/files[/:folder[/:filename]]", imersia_file_send, []},

            % Maptile server (content)
            {"/maptile/:folder/:zoom/:tilex/:tiley", imersia_maptile, []},

            % Authentication pages
            {"/auth/:provider/:command", imersia_login, []},

            % Secure websocket handlers
            {"/wotcha[/:userid]", imersia_companion_ssl_socket, []},

            % GeoJSON to be used by the WebApp from URLs
            {"/geojson[/:channelname[/:location]]", imersia_geojson_api, []}
        ] ++
        AdditionalHTTPSFolders ++ DefaultHTTPSFolder
        }
    ]),

    io:format("Imersia MRServer Version ~s~n", [imersia_settings:get_version()]),

    case imersia_settings:get_setting(mrserver, allowhttp) of
        <<"true">> ->
            DispatchHttp = cowboy_router:compile([
                {'_', [
                    % For SSL Certificate generation
                    {"/.well-known/acme-challenge/:challenge", acme_challenge, []},

                    % Log an event to a Geobot
                    {"/api/geobots/log", imersia_geobots_log_api, []},

                    % Send an event to a Geobots automations
                    {"/api/geobots/send", imersia_geobots_send_api, []},

                    % Files server (content)
                    {"/files[/:folder[/:filename]]", imersia_file_send, []},

                    % Unsecure websocket handler
                    {"/wotcha[/:userid]", imersia_companion_socket, []},

                    % Public geojson content
                    {"/geojson[/:channelname[/:location]]", imersia_geojson_api, []}
                ] ++
                AdditionalHTTPFolders ++ DefaultHTTPFolder
                }
            ]),

            {ok, _} = cowboy:start_clear(http, [{port, 80}], #{
                env => #{dispatch => DispatchHttp}
            });
        _ -> ok
    end,

    SSLDir = binary_to_list(imersia_settings:get_setting(mrserver, ssldir)),

    {ok, _CowboyPid} = cowboy:start_tls (https,
    [
        {port, 443},
        {certfile, SSLDir ++ "/mrserver.crt"},
        {keyfile, SSLDir ++ "/mrserver.key"},
        {cacertfile, SSLDir ++ "/intermediate.crt"},
        {versions, ['tlsv1.2', 'tlsv1.1']},
        {ciphers, ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
            "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
            "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
            "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
            "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
            "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
            "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
            "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
            "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
            "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
            "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
            "ECDH-RSA-AES128-SHA","AES128-SHA"]},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {max_connections, infinity}
    ], #{env => #{dispatch => Dispatch}}),

    imersia_db:init(),
    imersia_files:init(),

    mrserver_sup:start_link().
%% ----------------------------------------------------------------------------------------------------



%% ----------------------------------------------------------------------------------------------------
%% What to do when stopping.
%% ----------------------------------------------------------------------------------------------------
stop(_State) ->
	ok.
%% ----------------------------------------------------------------------------------------------------



%% ----------------------------------------------------------------------------------------------------
%% Utility Functions imersia_misc:basedir()
%% ----------------------------------------------------------------------------------------------------
process_folders_list(_, _, []) -> [];
process_folders_list(Base, Folder, [".gitignore" | Tail]) -> process_folders_list(Base, Folder, Tail);
process_folders_list(Base, Folder, [".DS_Store" | Tail]) -> process_folders_list(Base, Folder, Tail);
process_folders_list(Base, Folder, [Head | Tail]) ->
    case filelib:is_dir(Base ++ "/" ++ Folder ++ "/" ++ Head) of
        true ->
            case filelib:is_file(Base ++ "/" ++ Folder ++ "/" ++ Head ++ "/index.html") of
                true ->
                    [{"/" ++ Head, cowboy_static, {file, Base ++ "/" ++ Folder ++ "/" ++ Head ++ "/index.html"}}];
                false ->
                    []
            end ++
            [{"/" ++ Head ++ "/[...]", cowboy_static, {dir, Base ++ "/" ++ Folder ++ "/" ++ Head ++ "/"}}] ++
            process_folders_list(Base, Folder, Tail);
        false ->
            [{"/" ++ Head, cowboy_static, {file, Base ++ "/" ++ Folder ++ "/" ++ Head}}] ++
            process_folders_list(Base, Folder, Tail)
    end.

process_default_folder(_, _, "") -> [];
process_default_folder(Base, Folder, DefaultFolder) ->
    [
        {"/", cowboy_static, {file, Base ++ "/" ++ Folder ++ "/" ++ DefaultFolder ++ "/index.html"}},
        {"/[...]", cowboy_static, {file, Base ++ "/" ++ Folder ++ "/" ++ DefaultFolder ++ "/"}}
    ].
% ----------------------------------------------------------------------------------------------------
