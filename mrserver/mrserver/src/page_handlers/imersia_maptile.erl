% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Send Map tile
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_maptile).
-export([init/2]).

% ----------------------------------------------------------------------------------------------------
% Pass the maptile file referred to via the bindings through to the client
% ----------------------------------------------------------------------------------------------------
init(Req, State) ->

	% Extract the parts of the file details from the URI
	Folder = convert_binary_to_list(cowboy_req:binding(folder, Req)),
	Zoom = convert_binary_to_list(cowboy_req:binding(zoom, Req)),
	Tilex = convert_binary_to_list(cowboy_req:binding(tilex, Req)),
	Tiley = convert_binary_to_list(cowboy_req:binding(tiley, Req)),

	#{url := URLbin} = cowboy_req:match_qs([url], Req),
	URL = convert_binary_to_list(URLbin),

	% Construct the local filename
	Filename = imersia_misc:basedir() ++ "/maptile/" ++ Folder ++ "/" ++ Zoom ++ "/" ++ Tilex ++ "/" ++ Tiley ++ ".png",

	% Respond appropriately depending on whether the file exists or not.
	case check_if_image_valid(Filename) of
        ok -> send_maptile(Req, State, Filename);
        error ->
			case cache_locally(Filename, URL) of
				ok -> send_maptile(Req, State, Filename);
				error -> send_maptile(Req, State, imersia_misc:basedir() ++ "/https/images/grid.png")
			end
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Convert binary to list without crashing if parameter is null or undefined
% ----------------------------------------------------------------------------------------------------
convert_binary_to_list(undefined) -> undefined;
convert_binary_to_list(null) -> undefined;
convert_binary_to_list(Binary) -> binary_to_list(Binary).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Return true if the file exists and is a valid image, false otherwise
% ----------------------------------------------------------------------------------------------------
check_if_image_valid(Filename) ->
	% Does the file exist at all?
	case file:read_file_info(Filename) of
		{ok, _} -> check_if_is_valid_image(Filename);
		{error, _} -> error
	end.
check_if_is_valid_image(Filename) ->
	% Is it a valid image.  Sometimes the image downloader brings back an invalid image...
	case file:read_file(Filename) of
        {ok, FileData} ->
			try emagick:imageinfo(FileData) of
				{ok, _} -> ok;
				_ -> error
			catch
				_:_ -> file:delete(Filename), error
			end;
        _ ->
			error
    end.
% ----------------------------------------------------------------------------------------------------




% ----------------------------------------------------------------------------------------------------
% Send the given file to the client
% ----------------------------------------------------------------------------------------------------
send_maptile(Req, State, Filename) ->

	Mimetype = mimerl:filename(list_to_binary(Filename)),
	Req2 = case file:read_file(Filename) of
        {ok, FileData} ->
			cowboy_req:reply(200, #{<<"content-type">> => Mimetype, <<"access-control-allow-origin">> => <<"*">>}, FileData, Req);
        _ ->
			cowboy_req:reply(404, Req)
    end,

	{ok, Req2, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Save the tile segment as a local file to speed up loading later.
% ----------------------------------------------------------------------------------------------------
cache_locally(Filename, URL) ->
    % Cache the file locally
    case filelib:ensure_dir(Filename) of
        ok -> load_file(Filename, URL, 10);
        _ -> error
    end.

load_file(_, _, 0) -> error;
load_file(Filename, URL, Counter) ->
	case httpc:request(get, {URL, [{"connection", "close"}]}, [], [{sync, true}, {stream, Filename}]) of
		{ok, _} ->
			case check_if_image_valid(Filename) of
				ok -> ok;
				error -> timer:sleep(100), load_file(Filename, URL, Counter-1)
			end;
		_ -> file:delete(Filename), timer:sleep(100), load_file(Filename, URL, Counter-1)
	end.
% ----------------------------------------------------------------------------------------------------
