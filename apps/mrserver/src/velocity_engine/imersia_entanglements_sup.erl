% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: The Supervisor for the entanglements worker.
% ----------------------------------------------------------------------------------------------------
-module(imersia_entanglements_sup).
-behaviour(supervisor).

-include("../imersia_datatypes.hrl").

-export([start_link/1]).
-export([init/1]).

-export([start_entanglement/1, stop_entanglement/1]).



% ----------------------------------------------------------------------------------------------------
% Start the Supervisor
% ----------------------------------------------------------------------------------------------------
start_link(GeobotID) ->
    GeobotEntanglementsSupNameAtom = process_name(GeobotID),

    imersia_misc:debug(debug, "Starting a new Entanglements supervisor ~s~n", [GeobotEntanglementsSupNameAtom]),

    supervisor:start_link({local, GeobotEntanglementsSupNameAtom}, imersia_entanglements_sup, []).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
process_name(GeobotID) -> binary_to_atom(<< GeobotID/binary, "_entanglements_sup" >>, utf8).
worker_name(MRServer, RemoteGeobotID) -> binary_to_atom(<< RemoteGeobotID/binary, "@", MRServer/binary >>, utf8).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Called when Supervisor Started - defines the Workers
% ----------------------------------------------------------------------------------------------------
init(_Args) ->
    RestartStrategy = #{strategy => simple_one_for_one, intensity => 10, period => 50},
    ChildSpec = #{
        id => imersia_entanglements_worker_fsm,
        start => {imersia_entanglements_worker_fsm, start_link, []},
        type => worker,
        restart => permanent
    },
    {ok, {RestartStrategy, [ChildSpec]}}.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Start an entanglements worker given a set of parameters
% GeobotID, MRServer, DeveloperID, UserEmail, Password, GeoHash, RemoteGeobotID
% ----------------------------------------------------------------------------------------------------
start_entanglement(Parameters) ->
    #{mrserver := MRServer, geobotid := GeobotID, remotegeobotid := RemoteGeobotID} = Parameters,
    EntanglementProcessAtom = worker_name(MRServer, RemoteGeobotID),
    ParametersWithName = Parameters#{id => EntanglementProcessAtom},
    case whereis(EntanglementProcessAtom) of
        undefined ->
            Result = supervisor:start_child(process_name(GeobotID), [ParametersWithName]),
            case Result of
                {ok, Pid} -> {ok, {EntanglementProcessAtom, Pid}};
                _Error -> {error, start_child}
            end;
        Pid -> {ok, {EntanglementProcessAtom, Pid}}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Stop an entanglement process
% ----------------------------------------------------------------------------------------------------
stop_entanglement(Parameters) ->
    #{mrserver := MRServer, geobotid := GeobotID, remotegeobotid := RemoteGeobotID} = Parameters,
    EntanglementProcessAtom = worker_name(MRServer, RemoteGeobotID),
    case whereis(EntanglementProcessAtom) of
        undefined -> {ok, disentangled};
        Pid ->
            EntanglementProcessAtom ! {disentangle},
            supervisor:terminate_child(process_name(GeobotID), Pid),
            {ok, disentangled}
    end.
% ----------------------------------------------------------------------------------------------------
