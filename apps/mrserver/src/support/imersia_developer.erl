% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Check and log developer name and location
% TODO: Actually log the API calls in the Analytics Database
% ----------------------------------------------------------------------------------------------------

-module(imersia_developer).
-include("../imersia_datatypes.hrl").

-export([check_and_log/5]).



% ----------------------------------------------------------------------------------------------------
% Check the developerID and make sure it matches one of the ones allowed.
% And log the call in the Analytics database
% ----------------------------------------------------------------------------------------------------
check_and_log(undefined, _, _, _, _) -> {error, developerid};
check_and_log(_, undefined, _, _, _) -> {error, location};
check_and_log(_, _, undefined, _, _) -> {error, command};
check_and_log(_, _, _, undefined, _) -> {error, uri};
check_and_log(_, _, _, _, undefined) -> {error, parameters};
check_and_log(DeveloperID, Location, Command, URI, Parameters) ->
    ValidDeveloper = check_developerid(DeveloperID),
    log_transaction(DeveloperID, Location, Command, URI, Parameters, ValidDeveloper),
    if
        ValidDeveloper -> {ok, logged};
        true -> {error, developerid}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Log the transaction
% TODO: Use the local analytics database
% ----------------------------------------------------------------------------------------------------
log_transaction(DeveloperID, Location, Command, URI, Parameters, _ValidDeveloper) ->
    CurrentDate = iso8601:format(calendar:universal_time()),
    Parameters2 = maps:remove(connection, Parameters), % The connection parameter, if it exists, doesn't convert well to JSON
    ParametersBinary = try jiffy:encode(Parameters2) of
        Result -> Result
    catch
        _:_ -> <<"Error encoding parameters">>
    end,
    _Output = iolist_to_binary([CurrentDate, "|", DeveloperID, "|", Location, "|", Command, "|", URI, "|", ParametersBinary]),
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check this is a valid developerID based on the license code in the mrserver.toml file
% ----------------------------------------------------------------------------------------------------
check_developerid(DeveloperID) ->
    case imersia_settings:developerid() of
        undefined -> false;
        Set -> sets:is_element(DeveloperID, Set)
    end.
% ----------------------------------------------------------------------------------------------------
