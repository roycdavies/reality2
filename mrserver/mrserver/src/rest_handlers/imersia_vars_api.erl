% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Access to server-side variables from javascript
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_vars_api).

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    resource_exists/2,
    to_json/2
]).

% ----------------------------------------------------------------------------------------------------
% Initialise as a Cowboy Rest Handler
% ----------------------------------------------------------------------------------------------------
init(Req, State) -> {cowboy_rest, Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up the handlers
% ----------------------------------------------------------------------------------------------------
allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"GET">>], Req, State}.
content_types_provided(Req, State) -> {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Read parameters and check Authorization
% ----------------------------------------------------------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> {true, Req, State};
        _ ->
            % Collect common parameters
            Key = cowboy_req:header(<<"key">>, Req),
            Category = cowboy_req:header(<<"category">>, Req),

            URL = cowboy_req:header(<<"url">>, Req),
            Type = cowboy_req:header(<<"type">>, Req),
            GeobotID = cowboy_req:header(<<"geobotid">>, Req),
            ContextIDsRaw = cowboy_req:header(<<"contextids">>, Req),
            ContextIDs = imersia_misc:safely_decode_json(ContextIDsRaw, ContextIDsRaw),

            case check_parameters(Category, Key) of
                {error, Reason} ->
                    {{false, atom_to_list(Reason)} , Req, State};
                ok ->
                    % Add them to the Request passing through
                    Parameters = #{key => Key, category => Category, url => URL, type => Type, geobotid => GeobotID, contextids => ContextIDs},
                    Req2 = Req#{parameters => Parameters},

                    % Continue if this is a valid command for this developerID
                    DeveloperID = cowboy_req:header(<<"developerid">>, Req2),
                    Location = cowboy_req:header(<<"location">>, Req2),
                    case imersia_developer:check_and_log(DeveloperID, Location, cowboy_req:method(Req2), cowboy_req:uri(Req2), Parameters) of
                        {ok, logged} -> {true, Req2, State};
                        {error, Reason} ->
                            {{false, atom_to_list(Reason)} , Req2, State}
                    end
            end
    end.

check_parameters(<<"superadmin">>, _) -> {error, category};
check_parameters(<<"license">>, _) -> {error, category};
check_parameters(<<"billing">>, _) -> {error, category};
check_parameters(<<"smtp">>, _) -> {error, category};
check_parameters(<<"mnesia">>, _) -> {error, category};
check_parameters(<<"file">>, _) -> {error, category};
check_parameters(<<"database">>, _) -> {error, category};
check_parameters(<<"mrserver">>, <<"version">>) -> ok;
check_parameters(<<"mrserver">>, <<"url">>) -> ok;
check_parameters(<<"mrserver">>, <<"name">>) -> ok;
check_parameters(<<"mrserver">>, <<"debug">>) -> ok;
check_parameters(<<"mrserver">>, _) -> {error, key};
check_parameters(undefined, _) -> {error, category};
check_parameters(_, undefined) -> {error, key};
check_parameters(_, _) -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Determine if this variable actually exists or not
% ----------------------------------------------------------------------------------------------------
resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> ->
            {true, Req, State};
        _ ->
            #{parameters := #{key := Key, category := Category}} = Req,
            get_value(Category, Key, Req, State)
    end.

get_value(<<"licensor">>, <<"generate">>, Req, State) ->
    #{parameters := #{url := URL, type := Type, geobotid := GeobotID, contextids := ContextIDs}} = Req,
    LicenseCode = imersia_license:generate_license(URL, GeobotID, Type, ContextIDs),
    Req2 = Req#{parameters => #{value => LicenseCode}},
    {true, Req2, State};

get_value(Category, Key, Req, State) ->
    CategoryAtom = binary_to_atom(Category, utf8),
    KeyAtom = binary_to_atom(Key, utf8),
    case imersia_settings:get_setting(CategoryAtom, KeyAtom) of
        undefined ->
            imersia_misc:response_error(undefined, Req, State);
        Value ->
            Req2 = Req#{parameters => #{value => Value}},
            {true, Req2, State}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% The response to a GET or HEAD
% ----------------------------------------------------------------------------------------------------
to_json(Req, State) ->
    #{parameters := #{value := Value}} = Req,
    {jsx:encode([{value, Value}]), Req, State}.
% ----------------------------------------------------------------------------------------------------
