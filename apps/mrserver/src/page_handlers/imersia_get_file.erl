% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Send File from a Channel, Geobot or Companion
% TODO: Check authentication and developer details.
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_get_file).

-include("../imersia_datatypes.hrl").

-export([init/2]).

% ----------------------------------------------------------------------------------------------------
% Pass the file referred to via the bindings through to the client, processed as specified
% ----------------------------------------------------------------------------------------------------
init(Req, State) ->

	% Extract the parts of the file details from the URI
	Channel = cowboy_req:binding(channel, Req),
	Geobot = cowboy_req:binding(geobot, Req),
	Filename = get_filename(cowboy_req:binding(filename, Req)),

	send_file(Req, State, Channel, Geobot, Filename).

get_filename(undefined) -> <<"index.html">>;
get_filename(Filename) -> Filename.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Send the given file to the client
% ----------------------------------------------------------------------------------------------------
send_file(Req, State, undefined, _, _) ->
	{ok, cowboy_req:reply(404, Req), State};
send_file(Req, State, _, undefined, _) ->
	{ok, cowboy_req:reply(404, Req), State};
send_file(Req, State, _, _, undefined) ->
	{ok, cowboy_req:reply(404, Req), State};
send_file(Req, State, Channel, Geobot, Filename) ->
	Connection = imersia_db:connect(),
	Result = case imersia_db:channel_id(Connection, Channel) of
		{ok, ChannelID} ->
			case get_geobot_id_from_name(Connection, ChannelID, Geobot) of
				{ok, GeobotID} ->
					case imersia_files:download(GeobotID, Filename) of
						{ok, Mimetype, FileData} ->
							{ok, cowboy_req:reply(200, #{<<"content-type">> => Mimetype, <<"access-control-allow-origin">> => <<"$*">>}, FileData, Req), State};
						{error, undefined, reading} ->
							{ok, cowboy_req:reply(404, Req), State}
					end;
				_ -> {ok, cowboy_req:reply(404, Req), State}
			end;
		_ -> {ok, cowboy_req:reply(404, Req), State}
	end,
	imersia_db:close(Connection),
	Result.

get_geobot_id_from_name(Connection, ChannelID, Geobot) ->
	case imersia_db:geobot_list(Connection, ChannelID, undefined, false) of
		{ok, GeobotList} ->
			get_geobot_id_from_list(GeobotList, Geobot);
		_ -> {error, geobotid}
	end.

get_geobot_id_from_list([], _) -> {error, geobotid};
get_geobot_id_from_list([GeobotRecord | _], Geobot) when GeobotRecord#geobot.name == Geobot -> {ok, GeobotRecord#geobot.geobotid};
get_geobot_id_from_list([GeobotRecord | GeobotRecords], Geobot) when GeobotRecord#geobot.name =/= Geobot -> get_geobot_id_from_list(GeobotRecords, Geobot).
% ----------------------------------------------------------------------------------------------------
