% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Channel Handler for the API
%
% ----------------------------------------------------------------------------------------------------
-module(imersia_channels_api).

-include("../imersia_datatypes.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    to_json/2,
    from_json/2,
    resource_exists/2,
    delete_resource/2,
    delete_completed/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
content_types_accepted(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    % Collect common parameters
    Connection = imersia_db:connect(),
    % SessionID from the headers
    SessionID = cowboy_req:header(<<"sessionid">>, Req),
    % Name from the request
    Name = cowboy_req:binding(name, Req),
    % ChannelID extracted either from the name or headers
    ChannelID = get_channelid(Connection, cowboy_req:header(<<"channelid">>, Req), Name),
    % UserID extracted from the SessionID
    UserID = get_userid(Connection, SessionID),
    % OtherUserID may be used to get channels owned by a different user
    OtherUserID = case cowboy_req:header(<<"userid">>, Req) of undefined -> UserID; Value -> Value end,
    % ContextIDs define extra capabilities if not the owner of the channel(s) being interrogated
    ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
    ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

    % Add them to the Request passing through
    Parameters = #{connection => Connection, contextids => ContextIDs, sessionid => SessionID, channelid => ChannelID, userid => UserID, otheruserid => OtherUserID, name => Name},
    Req2 = Req#{parameters => Parameters},

    % Continue if this is a valid command for this developerID
    DeveloperID = cowboy_req:header(<<"developerid">>, Req),
    Location = cowboy_req:header(<<"location">>, Req),
    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req), cowboy_req:uri(Req), Parameters) of
        {ok, logged} ->
            % GET ChannelID == undefined, OtherUserID == undefined => Get list of channels for UserID
            % GET ChannelID == undefined, OtherUserID != undefined => Get list of channels for OtherUserID
            % GET ChannelID != undefined => Get details of specicified channel
            % POST/PUT ChannelID == undefined => Creating new channel UserID
            % POST/PUT ChannelID != undefined => Updating channel for UserID
            % DELETE ChannelID != undefined => Delete channel only if owned by logged in user
            Allowance = imersia_auth:check_channel(Connection, cowboy_req:method(Req), ChannelID, UserID, ContextIDs),
            {Parameters2, Response} = case Allowance of
                {ok, Action} ->
                    { Parameters#{action => Action}, true };
                {error, Reason} ->
                    imersia_db:close(Connection),
                    { Parameters, {false, atom_to_list(Reason)} }
            end,
            Req3 = Req2#{parameters => Parameters2},
            {Response, Req3, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            {{false, atom_to_list(Reason)} , Req2, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check the SessionID, and then the ChannelID.
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    #{parameters := #{action := Action}} = Req,
    do_resource_exists(Action, Req, State).

do_resource_exists(head, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, channelid := ChannelID}} = Req,

    Result = imersia_db:channel_exists(Connection, ChannelID),
    case Result of
        {ok, exists} -> {true, Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;

do_resource_exists(delete, Req, State) ->
    % If we've got this far, we already know it exists
    {true, Req, State};

do_resource_exists(get, Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, channelid := ChannelID}} = Req,
    Result = imersia_db:channel_exists(Connection, ChannelID),
    case Result of
        {ok, exists} -> {true, Req, State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end;

do_resource_exists(put, Req, State) ->
    #{parameters := #{connection := Connection, userid := UserID}} = Req,
    if
        (UserID == undefined) ->
            imersia_db:close(Connection),
            imersia_misc:response_error(sessionid, Req, State);
        true ->
            {true, Req, State}
    end;

do_resource_exists(post, Req, State) ->
    #{parameters := #{connection := Connection, userid := UserID}} = Req,
    if
        (UserID == undefined) ->
            imersia_db:close(Connection),
            imersia_misc:response_error(sessionid, Req, State);
        true ->
            {true, Req, State}
    end;

do_resource_exists(channellist, Req, State) ->
    #{parameters := #{connection := Connection, otheruserid := OtherUserID}} = Req,
    if
        (OtherUserID == undefined) ->
            imersia_db:close(Connection),
            imersia_misc:response_error(userid, Req, State);
        true ->
            {true, Req, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the ChannelID either from the Header or by checking the name in the URI
% ----------------------------------------------------------------------------------------------------
get_channelid(_, undefined, undefined) -> undefined;
get_channelid(_, ChannelID, undefined) -> ChannelID;
get_channelid(Connection, _, Name) ->
    case imersia_db:channel_id(Connection, Name) of
        {ok, ChannelID} -> ChannelID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Get the UserID from the SessionID
% ----------------------------------------------------------------------------------------------------
get_userid(_, undefined) -> undefined;
get_userid(Connection, SessionID) ->
    case imersia_db:user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} -> UserID;
        _ -> undefined
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET or HEAD
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    % Pick up the values from the Request
    #{parameters := #{connection := Connection, action := Action, contextids := ContextIDs, channelid := ChannelID, userid := UserID, otheruserid := OtherUserID}} = Req,

    case Action of
        head ->
            imersia_db:close(Connection),
            {"", Req, State};

        channellist ->
            % Get a list of Channels since no specific channel has been given either by name or ChannelID
            % Either it is a list of channels for the logged in user, or a different user
            ShowHidden = case cowboy_req:header(<<"showhidden">>, Req) of undefined -> true; Value2 -> binary_to_boolean(Value2) end,
            case imersia_db:channel_list(Connection, OtherUserID, UserID, ShowHidden, ContextIDs) of
                % Return the list of channels
                {ok, ChannelList} ->
                    SortFun = fun(A, B) -> A#channel.name < B#channel.name end,
                    SortedChannelList = lists:sort(SortFun, ChannelList),
                    imersia_db:close(Connection),
                    {imersia_misc:safely_encode_json(convert_channels_to_json(SortedChannelList)), Req, State};
                % Some odd error occurred
                {error, Reason1} ->
                    imersia_db:close(Connection),
                    {jsx:encode([{error, Reason1}]), Req, State}
            end;

        get ->
            % Get a single Channel's details
            Result = imersia_db:channel_getdetails(Connection, ChannelID),
            case Result of
                {ok, Channel} ->
                    ChannelJSON = imersia_misc:record_to_json(Channel, false),
                    imersia_db:close(Connection),
                    {imersia_misc:safely_encode_json(ChannelJSON), Req, State};
                % Some odd error occurred
                {error, Reason2} ->
                    imersia_db:close(Connection),
                    {jsx:encode([{error, Reason2}]), Req, State}
            end
    end.

convert_channels_to_json([]) -> [];
convert_channels_to_json([Head | Tail]) ->
    [imersia_misc:record_to_json(Head, false) | convert_channels_to_json(Tail)].

binary_to_boolean(<<"true">>) -> true;
binary_to_boolean(<<"false">>) -> false;
binary_to_boolean(true) -> true;
binary_to_boolean(false) -> false.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a POST or PUT
% ----------------------------------------------------------------------------------------------------
from_json(Req, State) ->
    % Pick up the values from the request
    #{parameters := #{connection := Connection, action := Action, channelid := ChannelID, userid := UserID, name := URLName}} = Req,

    % Grab the new or updated Channel details from the body
    Body = imersia_misc:interpret_body(cowboy_req:has_body(Req), Req),
    case imersia_misc:json_to_record(channel, Body) of
        error ->
            imersia_db:close(Connection),
            imersia_misc:response_error(channel, Req, State);
        Channel ->
            % Get the name from either the URI or the details sent in
            Name = get_name(URLName, Channel#channel.name),

            % case UserID of
            %     % The user has to be logged in
            %     undefined ->
            %         imersia_db:close(Connection),
            %         imersia_misc:response_error(sessionid, Req, State);
            %
            %     % Either create a new Channel or update an existing one
            %     _ ->
                    case Action of
                        post ->
                            % This must be a POST, so create a new channel
                            Result = imersia_db:channel_new(Connection, UserID, Name, Channel),
                            case Result of
                                {ok, NewChannelID} ->
                                    imersia_db:close(Connection),
                                    {true, cowboy_req:set_resp_body([<<"{\"channelid\":\"">>, NewChannelID, <<"\"}">>], Req), State};
                                {error, Reason} ->
                                    imersia_db:close(Connection),
                                    imersia_misc:response_error(Reason, Req, State)
                            end;
                        put ->
                            % This must be a PUT, so update an existing channel
                            if
                                % Change the name of the Channel
                                ((Channel#channel.name =/= undefined) and (Channel#channel.name =/= null)) ->
                                    Result2 = imersia_db:channel_rename(Connection, ChannelID, Channel#channel.name),
                                    case Result2 of
                                        {ok, updated} ->
                                            % Update other aspects of the Channel
                                            Result3 = imersia_db:channel_setdetails(Connection, ChannelID, Channel),
                                            case Result3 of
                                                {ok, updated} ->
                                                    imersia_db:close(Connection),
                                                    {true, cowboy_req:set_resp_body([<<"{\"channelid\":\"">>, ChannelID, <<"\"}">>], Req), State};
                                                {error, Reason3} ->
                                                    imersia_db:close(Connection),
                                                    imersia_misc:response_error(Reason3, Req, State)
                                            end;
                                        {error, Reason2} ->
                                            imersia_db:close(Connection),
                                            imersia_misc:response_error(Reason2, Req, State)
                                    end;
                                true ->
                                    % Not changing the name, so just update other aspects of the Channel
                                    Result4 = imersia_db:channel_setdetails(Connection, ChannelID, Channel),
                                    case Result4 of
                                        {ok, updated} ->
                                            imersia_db:close(Connection),
                                            {true, cowboy_req:set_resp_body([<<"{\"channelid\":\"">>, ChannelID, <<"\"}">>], Req), State};
                                        {error, Reason4} ->
                                            imersia_db:close(Connection),
                                            imersia_misc:response_error(Reason4, Req, State)
                                    end
                            end
                    end
            % end
    end.

% Get the name either from the URL or the Channel Details in the body
get_name(undefined, RecordName) -> RecordName;
get_name(URLName, _) -> URLName.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a Channel
% Only the owner can delete the channel, so no need to look at ContextIDs - and have only got this
% far if logged in user is owner of channel
% ----------------------------------------------------------------------------------------------------
delete_resource(Req, State) ->
    % Grab the parameters
    #{parameters := #{connection := Connection, channelid := ChannelID}} = Req,

    % Delete the Channel
    case imersia_db:channel_delete(Connection, ChannelID) of
        {ok, deleted} ->
            imersia_db:close(Connection),
            {true, cowboy_req:set_resp_body([<<"{\"channelid\":\"">>, ChannelID, <<"\"}">>], Req), State};
        {error, Reason} ->
            imersia_db:close(Connection),
            imersia_misc:response_error(Reason, Req, State)
    end.
delete_completed(Req, State) ->
    {true, Req, State}.
% ----------------------------------------------------------------------------------------------------
