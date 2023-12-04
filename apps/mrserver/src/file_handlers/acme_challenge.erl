% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(acme_challenge).
-include_lib("kernel/include/file.hrl").

-export([init/2]).

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
init(Req, State) ->
	% Get the challenge file name
	Challenge = erlang:binary_to_list(cowboy_req:binding(challenge, Req)),

	SSLDir = binary_to_list(imersia_settings:get_setting(mrserver, ssldir)),

	Mimetype = case imersia_settings:get_setting(acme_challenge, mimetype) of
		undefined -> <<"text/plain">>;
		V2 -> V2
	end,

	Filename = SSLDir ++ "/acme-challenge/" ++ Challenge,

	erlang:display(Filename),
	erlang:display(Mimetype),

    send_file(Req, State, Filename, Mimetype).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
send_file(Req, State, Filename, Mimetype) ->

	Req2 = case file:read_file(Filename) of
        {ok, FileData} ->
			cowboy_req:reply(200, #{<<"content-type">> => Mimetype}, FileData, Req);
        Reason -> erlang:display(Reason),
			cowboy_req:reply(404, Req)
    end,

	{ok, Req2, State}.
% ----------------------------------------------------------------------------------------------------
