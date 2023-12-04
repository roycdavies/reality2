% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Database abstract data structure
% ----------------------------------------------------------------------------------------------------

%% @private

-module(imersia_db).

% General data types for use in the database and elsewhere
-include("../imersia_datatypes.hrl").
-include("imersia_db.hrl").

-export([init/0, drop/0, connect/0, close/1, new_id/0, create_statid/3, user_exists_and_matches/4]).
-export([user_id/2, user_email/2, user_id_from_sessionid/2, user_new/5, user_list/1, user_login/4, user_logout/2, user_delete/3, user_session_new/3,
         user_session_exists/3, user_session_new/4, user_get_details/2, user_get_details_by_userid/2, user_set_details/3, user_change_password/3]).
-export([channel_exists/2, channel_new/4, channel_new/5, channel_rename/3, channel_id/2, channel_list/4, channel_list/5, channel_list/1, channel_getdetails/2, channel_getdetails/3, channel_setdetails/3, channel_delete/2, channel_delete_all/2]).
-export([geobot_exists/2, geobot_getchannelid/2, geobot_new/4, geobot_new/5, geobot_list/4, geobot_list/1, geobot_list/6, geobot_getdetails/2, geobot_getdetails/3, geobot_setdetails/3, geobot_delete/3, geobot_delete_all/2]).
-export([metadata_init/2, metadata_exists/3, metadata_drop/2, metadata_id/3, metadata_list/2, metadata_get/3, metadata_set/4, metadata_delete/3, metadata_get_by_id/3, metadata_set_by_id/4, metadata_delete_by_id/3, metadata_delete_all/2]).
-export([automation_init/2, automation_exists/3, automation_drop/2, automation_id/3, automation_new/3, automation_list/2, automation_getdetails/3, automation_getcommands/2, automation_setdetails/4, automation_delete/3, automation_delete_all/2]).
-export([analytics_log/6, analytics_query/5]).
-export([context_init/2, context_exists/3, context_is_allowed/4, context_drop/2, context_list/2, context_get/3, context_set/4, context_delete/3, context_delete_all/2]).
-export([passcode_new/2, passcode_check/3]).

% ----------------------------------------------------------------------------------------------------
% Abstract functions to be implemented in the specific implementation
% ----------------------------------------------------------------------------------------------------
impl(Sub)                                                   -> {ok, Module} = application:get_env(mrserver, Sub), Module.
init()                                                      -> set_settings(), apply(impl(db_impl), init, []).
connect()                                                   -> apply(impl(db_impl), connect, []).
close(Connection)                                           -> apply(impl(db_impl), close, [Connection]).
drop()                                                      -> apply(impl(db_impl), drop, []).

user_id(Connection, UserEmail)                              -> apply(impl(db_impl_user), user_id, [Connection, UserEmail]).
user_email(Connection, UserID)                              -> apply(impl(db_impl_user), user_email, [Connection, UserID]).
user_id_from_sessionid(Connection, SessionID)               -> apply(impl(db_impl_user), user_id_from_sessionid, [Connection, SessionID]).
user_new(Connection, UserEmail, Password, UserDetails, Location)
                                                            -> user_new_and_start_worker(Connection, UserEmail, Password, UserDetails, Location).
user_list(Connection)                                       -> apply(impl(db_impl_user), user_list, [Connection]).
user_login(Connection, UserID, UserEmail, Password)         -> apply(impl(db_impl_user), user_login, [Connection, UserID, UserEmail, Password]).
user_logout(Connection, SessionID)                          -> apply(impl(db_impl_user), user_logout, [Connection, SessionID]).
user_delete(Connection, SessionID, UserID)                  -> user_delete_and_kill_worker(Connection, SessionID, UserID) .
user_session_new(Connection, Token, UserEmail)              -> apply(impl(db_impl_user), user_session_new, [Connection, Token, UserEmail]).
user_session_new(Connection, Token, UserEmail, Age)         -> apply(impl(db_impl_user), user_session_new, [Connection, Token, UserEmail, Age]).
user_session_exists(Connection, SessionID, UserID)          -> apply(impl(db_impl_user), user_session_exists, [Connection, SessionID, UserID]).
user_get_details(Connection, SessionID)                     -> apply(impl(db_impl_user), user_get_details, [Connection, SessionID]).
user_get_details_by_userid(Connnection, UserID)             -> apply(impl(db_impl_user), user_get_details_by_userid, [Connnection, UserID]).
user_set_details(Connection, SessionID, UserDetails)        -> user_set_and_notify_worker(Connection, SessionID, UserDetails).
user_change_password(Connection, SessionID, NewPassword)    -> apply(impl(db_impl_user), user_change_password, [Connection, SessionID, NewPassword]).
% ----------------------------------------------------------------------------------------------------
% Channel
% ----------------------------------------------------------------------------------------------------
channel_exists(Connection, ChannelID)                       -> apply(impl(db_impl_channel), channel_exists, [Connection, ChannelID]).
channel_new(Connection, UserID, Name, Details)              -> channel_new_and_start_worker(Connection, UserID, Name, Details).
channel_new(Connection, UserID, ChannelID, Name, Details)   -> apply(impl(db_impl_channel), channel_new, [Connection, UserID, ChannelID, Name, Details]).
channel_rename(Connection, ChannelID, Name)                 -> channel_rename_and_notify_worker(Connection, ChannelID, Name).
channel_id(Connection, Name)                                -> apply(impl(db_impl_channel), channel_id, [Connection, Name]).
channel_list(Connection, ChannelID, VisitorID, ShowHidden)  -> apply(impl(db_impl_channel), channel_list, [Connection, ChannelID, VisitorID, ShowHidden]).
channel_list(Connection, ChannelID, VisitorID, ShowHidden, ContextIDs)
                                                            -> apply(impl(db_impl_channel), channel_list, [Connection, ChannelID, VisitorID, ShowHidden, ContextIDs]).
channel_list(Connection)                                    -> apply(impl(db_impl_channel), channel_list, [Connection]).
channel_getdetails(Connection, ChannelID)                   -> apply(impl(db_impl_channel), channel_getdetails, [Connection, ChannelID]).
channel_getdetails(Connection, ChannelID, VisitorID)        -> apply(impl(db_impl_channel), channel_getdetails, [Connection, ChannelID, VisitorID]).
channel_setdetails(Connection, ChannelID, Details)          -> channel_set_and_notify_worker(Connection, ChannelID, Details).
channel_delete(Connection, ChannelID)                       -> channel_delete_and_kill_worker(Connection, ChannelID).
channel_delete_all(Connection, UserID)                      -> apply(impl(db_impl_channel), channel_delete_all, [Connection, UserID]).
% ----------------------------------------------------------------------------------------------------
% Geobot
% ----------------------------------------------------------------------------------------------------
geobot_exists(Connection, GeobotID)                         -> apply(impl(db_impl_geobot), geobot_exists, [Connection, GeobotID]).
geobot_getchannelid(Connection, GeobotID)                   -> apply(impl(db_impl_geobot), geobot_getchannelid, [Connection, GeobotID]).
geobot_new(Connection, ChannelID, UserID, Details)          -> geobot_new_and_start_worker(Connection, ChannelID, UserID, Details).
geobot_new(Connection, ChannelID, UserID, GeobotID, Details)-> apply(impl(db_impl_geobot), geobot_new, [Connection, ChannelID, UserID, GeobotID, Details]).
geobot_list(Connection, ChannelID, VisitorID, ShowHidden)   -> apply(impl(db_impl_geobot), geobot_list, [Connection, ChannelID, VisitorID, ShowHidden]).
geobot_list(Connection)                                     -> apply(impl(db_impl_geobot), geobot_list, [Connection]).
geobot_list(Connection, ChannelID, VisitorID, Location, Radius, ShowHidden)
                                                            -> apply(impl(db_impl_geobot), geobot_list, [Connection, ChannelID, VisitorID, Location, Radius, ShowHidden]).
geobot_getdetails(Connection, GeobotID)                     -> apply(impl(db_impl_geobot), geobot_getdetails, [Connection, GeobotID]).
geobot_getdetails(Connection, GeobotID, VisitorID)          -> apply(impl(db_impl_geobot), geobot_getdetails, [Connection, GeobotID, VisitorID]).
geobot_setdetails(Connection, GeobotID, Details)            -> geobot_set_and_notify_worker(Connection, GeobotID, Details).
geobot_delete(Connection, ChannelID, GeobotID)              -> geobot_delete_and_kill_worker(Connection, ChannelID, GeobotID).
geobot_delete_all(Connection, ChannelID)                    -> apply(impl(db_impl_geobot), geobot_delete_all, [Connection, ChannelID]).
% ----------------------------------------------------------------------------------------------------
% Metadata
% ----------------------------------------------------------------------------------------------------
metadata_init(Connection, ID)                               -> apply(impl(db_impl_metadata), metadata_init, [Connection, ID]).
metadata_exists(Connection, ID, MetadataID)                 -> apply(impl(db_impl_metadata), metadata_exists, [Connection, ID, MetadataID]).
metadata_drop(Connection, ID)                               -> apply(impl(db_impl_metadata), metadata_drop, [Connection, ID]).
metadata_id(Connection, ID, Key)                            -> apply(impl(db_impl_metadata), metadata_id, [Connection, ID, Key]).
metadata_list(Connection, ID)                               -> apply(impl(db_impl_metadata), metadata_list, [Connection, ID]).
metadata_get(Connection, ID, Key)                           -> apply(impl(db_impl_metadata), metadata_get, [Connection, ID, Key]).
metadata_set(Connection, ID, Key, Value)                    -> metadata_set_and_notify_worker(Connection, ID, Key, Value).
metadata_delete(Connection, ID, Key)                        -> metadata_delete_and_notify_worker(Connection, ID, Key).
metadata_get_by_id(Connection, ID, MetadataID)              -> apply(impl(db_impl_metadata), metadata_get_by_id, [Connection, ID, MetadataID]).
metadata_set_by_id(Connection, ID, MetadataID, Value)       -> metadata_set_by_id_and_notify_worker(Connection, ID, MetadataID, Value).
metadata_delete_by_id(Connection, ID, MetadataID)           -> metadata_delete_by_id_and_notify_worker(Connection, ID, MetadataID).
metadata_delete_all(Connection, ID)                         -> apply(impl(db_impl_metadata), metadata_delete_all, [Connection, ID]).
% ----------------------------------------------------------------------------------------------------
% Automations
% ----------------------------------------------------------------------------------------------------
automation_init(Connection, GeobotID)                       -> apply(impl(db_impl_automation), automation_init, [Connection, GeobotID]).
automation_exists(Connection, GeobotID, AutomationID)       -> apply(impl(db_impl_automation), automation_exists, [Connection, GeobotID, AutomationID]).
automation_drop(Connection, GeobotID)                       -> apply(impl(db_impl_automation), automation_drop, [Connection, GeobotID]).
automation_new(Connection, GeobotID, Automation)            -> automation_new_and_notify_worker(Connection, GeobotID, Automation).
automation_id(Connection, GeobotID, Name)                   -> apply(impl(db_impl_automation), automation_id, [Connection, GeobotID, Name]).
automation_list(Connection, GeobotID)                       -> apply(impl(db_impl_automation), automation_list, [Connection, GeobotID]).
automation_getdetails(Connection, GeobotID, AutomationID)   -> apply(impl(db_impl_automation), automation_getdetails, [Connection, GeobotID, AutomationID]).
automation_getcommands(Connection, GeobotID)                -> apply(impl(db_impl_automation), automation_getcommands, [Connection, GeobotID]).
automation_setdetails(Connection, GeobotID, AutomationID, Automation)
                                                            -> automation_set_and_notify_worker(Connection, GeobotID, AutomationID, Automation).
automation_delete(Connection, GeobotID, AutomationID)       -> automation_delete_and_notify_worker(Connection, GeobotID, AutomationID).
automation_delete_all(Connection, GeobotID)                 -> apply(impl(db_impl_automation), automation_delete_all, [Connection, GeobotID]).
% ----------------------------------------------------------------------------------------------------
% Analytics
% ----------------------------------------------------------------------------------------------------
analytics_log(Connection, ID, Event, Location, Date, Parameters) -> apply(impl(db_impl_analytics), analytics_log, [Connection, ID, Event, Location, Date, Parameters]).
analytics_query(Connection, ID, Event, StartDate, EndDate)  -> apply(impl(db_impl_analytics), analytics_query, [Connection, ID, Event, StartDate, EndDate]).
% ----------------------------------------------------------------------------------------------------
% Context
% ----------------------------------------------------------------------------------------------------
context_init(Connection, ID)                                -> apply(impl(db_impl_context), context_init, [Connection, ID]).
context_exists(Connection, ID, ContextID)                   -> apply(impl(db_impl_context), context_exists, [Connection, ID, ContextID]).
context_is_allowed(Connection, ID, ContextIDs, Attribute)   -> check_context_is_allowed(Connection, ID, ContextIDs, Attribute).
context_drop(Connection, ID)                                -> apply(impl(db_impl_context), context_drop, [Connection, ID]).
context_list(Connection, ID)                                -> apply(impl(db_impl_context), context_list, [Connection, ID]).
context_get(Connection, ID, ContextID)                      -> apply(impl(db_impl_context), context_get, [Connection, ID, ContextID]).
context_set(Connection, ID, ContextID, Attributes)          -> context_set_and_notify_worker(Connection, ID, ContextID, Attributes).
context_delete(Connection, ID, ContextID)                   -> context_delete_and_notify_worker(Connection, ID, ContextID).
context_delete_all(Connection, ID)                          -> apply(impl(db_impl_context), metadata_delete_all, [Connection, ID]).
% ----------------------------------------------------------------------------------------------------
% Passcode
% ----------------------------------------------------------------------------------------------------
passcode_new(Connection, UserEmail)                         -> apply(impl(db_impl_passcode), passcode_new, [Connection, UserEmail]).
passcode_check(Connection, UserEmail, Passcode)             -> apply(impl(db_impl_passcode), passcode_check, [Connection, UserEmail, Passcode]).
% ----------------------------------------------------------------------------------------------------
% Generic functions useful and common across all implementations
% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
% Create a new ID for use in the tables as a channel, owner or Geobot ID.
% ----------------------------------------------------------------------------------------------------
new_id() ->
    quickrand:seed(),
    {UUID_binary, _} = uuid:get_v1(uuid:new(self())),
    list_to_binary(uuid:uuid_to_string(UUID_binary)).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Set up environment variables to define the current database implementation
% ----------------------------------------------------------------------------------------------------
set_settings() ->
    DBImplementation = erlang:binary_to_list(imersia_settings:get_setting(database, implementation)),
    application:set_env(mrserver, db_impl, list_to_atom("imersia_" ++ DBImplementation), [{persistent, true}]),
    application:set_env(mrserver, db_impl_analytics, list_to_atom("imersia_" ++ DBImplementation ++ "_analytics"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_channel, list_to_atom("imersia_" ++ DBImplementation ++ "_channel"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_geobot, list_to_atom("imersia_" ++ DBImplementation ++ "_geobot"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_metadata, list_to_atom("imersia_" ++ DBImplementation ++ "_metadata"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_automation, list_to_atom("imersia_" ++ DBImplementation ++ "_automation"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_user, list_to_atom("imersia_" ++ DBImplementation ++ "_user"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_context, list_to_atom("imersia_" ++ DBImplementation ++ "_context"), [{persistent, true}]),
    application:set_env(mrserver, db_impl_passcode, list_to_atom("imersia_" ++ DBImplementation ++ "_passcode"), [{persistent, true}]).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Create a semi-unique, efficient and searchable ID based on information from the event being stored.  The format is
% YYYYMMDDHH-GGGGGGGG-EVENT where
% - YYYYMMDDHH is the date in Hexadecimal
% - GGGGGGGG is the first 8 characters of the GeoHashed location
% - Event is the event
% All are converted to lowercase
% ----------------------------------------------------------------------------------------------------
create_statid(Event, Geohash, Date) ->
    % Grab the first 8 characters of the Geohash
    <<GeohashPart:8/binary, _/binary>> = Geohash,
    % Make the event text lowercase
    EventLower = string:lowercase(Event),
    % Ensure the Location has a correct Lat and Lng
    Location = imersia_misc:unify_location(null, null, 0, GeohashPart),
    % Make the Geohash lowercase
    GeohashPartLower = string:lowercase(GeohashPart),
    % Grab the date
    {{Year, Month, Day}, {Hour, _Minute, _Second}} = Date,
    % Convert the parts of the date to Hex
    YearBin = convert2hex(Year, 4),
    MonthBin = convert2hex(Month, 2),
    DayBin = convert2hex(Day, 2),
    HourBin = convert2hex(Hour, 2),
    % Construct the date to lowercase
    ShortDate = iso8601:format({{Year, Month, Day}, {Hour, 0, 0}}),
    DatePart = string:lowercase(<<YearBin/binary, MonthBin/binary, DayBin/binary, HourBin/binary>>),
    % Return the statid and other useful values
    {<<DatePart/binary, "-", GeohashPartLower/binary, "-", EventLower/binary>>, Location, ShortDate, EventLower}.

% Convert an integer to Hex, and pad to the specified Length
convert2hex(Value, Length) ->
    Zeroes = <<"000000">>,
    ValueBin = integer_to_binary(Value, 10),
    LengthZeroes = Length - byte_size(ValueBin),
    <<Zeroes:LengthZeroes/binary, ValueBin/binary>>.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% User commands that also notify Workers.
% ----------------------------------------------------------------------------------------------------
user_new_and_start_worker(Connection, UserEmail, Password, UserDetails, Location) ->
    Result = apply(impl(db_impl_user), user_new, [Connection, UserEmail, Password, UserDetails, Location]),
    case Result of
        {ok, UserID} -> _ = imersia_companion_sup:start_companion(UserID), Result;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
user_delete_and_kill_worker(Connection, SessionID, UserID) ->
    case imersia_files:delete_all_files(UserID) of
        {ok, deleted} ->
            Result = apply(impl(db_impl_user), user_delete, [Connection, SessionID, UserID]),
            case Result of
                {ok, deleted} -> _ = imersia_companion_sup:kill_companion(UserID), Result;
                _ -> Result
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
user_set_and_notify_worker(Connection, SessionID, UserDetails) ->
    Result = apply(impl(db_impl_user), user_set_details, [Connection, SessionID, UserDetails]),
    case (Result) of
        {ok, updated} ->
            {ok, UserID} = imersia_db:user_id_from_sessionid(Connection, SessionID),
            _ = imersia_companion_sup:notify_companion(UserID, {update, #user{userid=UserID, details=UserDetails}}),
            Result;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Channel commands that also notify workers
% ----------------------------------------------------------------------------------------------------
channel_new_and_start_worker(Connection, UserID, Name, Details) ->
    Result = apply(impl(db_impl_channel), channel_new, [Connection, UserID, Name, Details]),
    case Result of
        {ok, ChannelID} ->
            case apply(impl(db_impl_channel), channel_getdetails, [Connection, ChannelID]) of
                {ok, Channel} ->
                    _ = imersia_companion_sup:notify_companion(UserID, {channelnew, Channel}),
                    _ = imersia_channel_sup:start_channel(ChannelID),
                    Result;
                    % case imersia_settings:get_setting(mrserver, url) of
                    %     undefined -> ok;
                    %     URL ->
                    %         FullURL = << "https://", URL/binary, "/", Name/binary >>,
                    %         QRCode = qrcode:encode(FullURL),
                    %         QRImage = imersia_misc:simple_png_encode(QRCode),
                    %         imersia_files:upload(ChannelID, "qrcode.png", "image/png", QRImage)
                    % end;
                _ -> Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
channel_rename_and_notify_worker(Connection, ChannelID, Name) ->
    Result = apply(impl(db_impl_channel), channel_rename, [Connection, ChannelID, Name]),
    case Result of
        {ok, updated} ->
            case apply(impl(db_impl_channel), channel_getdetails, [Connection, ChannelID]) of
                {ok, Channel} ->
                    imersia_channel_sup:notify_channel(ChannelID, {rename, Channel#channel.name}),
                    Result;
                _ -> Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
channel_set_and_notify_worker(Connection, ChannelID, Details) ->
    Result = apply(impl(db_impl_channel), channel_setdetails, [Connection, ChannelID, Details]),
    case Result of
        {ok, updated} ->
            case apply(impl(db_impl_channel), channel_getdetails, [Connection, ChannelID]) of
                {ok, Channel} ->
                    imersia_channel_sup:notify_channel(ChannelID, {update, Channel}),
                    Result;
                _ -> Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
channel_delete_and_kill_worker(Connection, ChannelID) ->
    case imersia_files:delete_all_files(ChannelID) of
        {ok, deleted} ->
            Result = apply(impl(db_impl_channel), channel_delete, [Connection, ChannelID]),
            case Result of
                {ok, deleted} ->
                    _ = imersia_channel_sup:notify_channel(ChannelID, {delete, ChannelID}),
                    _ = imersia_channel_sup:kill_channel(ChannelID),
                    Result;
                _ -> Result
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Geobot commands that also notify workers
% ----------------------------------------------------------------------------------------------------
geobot_new_and_start_worker(Connection, ChannelID, UserID, Details) ->
    Result = apply(impl(db_impl_geobot), geobot_new, [Connection, ChannelID, UserID, Details]),
    case Result of
        {ok, GeobotID} ->
            case apply(impl(db_impl_geobot), geobot_getdetails, [Connection, GeobotID]) of
                {ok, Geobot} ->
                    _ = imersia_channel_sup:notify_channel(ChannelID, {geobotnew, Geobot}),
                    _ = imersia_geobot_sup_sup:start_geobot(GeobotID),
                    Result;
                _ -> Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
geobot_set_and_notify_worker(Connection, GeobotID, Details) ->
    Result = apply(impl(db_impl_geobot), geobot_setdetails, [Connection, GeobotID, Details]),
    % check_automation_and_update(GeobotID, Details#geobot.automations),
    case Result of
        {ok, updated} ->
            case apply(impl(db_impl_geobot), geobot_getdetails, [Connection, GeobotID]) of
                {ok, Geobot} ->
                    imersia_channel_sup:notify_channel(Geobot#geobot.channelid, {geobotupdate, Geobot}),
                    imersia_geobot_sup_sup:notify_geobot(GeobotID, {update, Geobot}),
                    Result;
                _ -> Result
            end;
        _ -> Result
    end.
%
% check_automation_and_update(_, null) -> ok;
% check_automation_and_update(GeobotID, Automations) ->
%     imersia_automations_sup:update_automations(GeobotID, Automations).
% ----------------------------------------------------------------------------------------------------
geobot_delete_and_kill_worker(Connection, ChannelID, GeobotID) ->
    case imersia_files:delete_all_files(GeobotID) of
        {ok, deleted} ->
            Result = apply(impl(db_impl_geobot), geobot_delete, [Connection, ChannelID, GeobotID]),
            case Result of
                {ok, deleted} ->
                    _ = imersia_channel_sup:notify_channel(ChannelID, {geobotdelete, GeobotID}),
                    _ = imersia_geobot_sup_sup:notify_geobot(GeobotID, {delete, GeobotID}),
                    _ = imersia_geobot_sup_sup:kill_geobot(GeobotID),
                    Result;
                _ -> Result
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Metadata commands that also notify workers
% ----------------------------------------------------------------------------------------------------
metadata_set_and_notify_worker(Connection, ID, Key, Value) ->
    Result = apply(impl(db_impl_metadata), metadata_set, [Connection, ID, Key, Value]),
    case Result of
        {ok, MetadataID} ->
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ -> EntityIDAtom ! {metadata, #metadata{key=Key, value=Value, metadataid=MetadataID}}, Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
metadata_set_by_id_and_notify_worker(Connection, ID, MetadataID, Value) ->
    Result = apply(impl(db_impl_metadata), metadata_set_by_id, [Connection, ID, MetadataID, Value]),
    case Result of
        {ok, updated} ->
            {ok, Metadata} = imersia_db:metadata_get_by_id(Connection, ID, MetadataID),
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ -> EntityIDAtom ! {metadata, Metadata}, Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
metadata_delete_and_notify_worker(Connection, ID, Key) ->
    Result = apply(impl(db_impl_metadata), metadata_delete, [Connection, ID, Key]),
    case Result of
        {ok, deleted} ->
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ ->
                    case metadata_get(Connection, ID, Key) of
                        {ok, Metadata} -> EntityIDAtom ! {metadatadelete, Metadata#metadata.metadataid}, Result;
                        _ -> Result
                    end
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
metadata_delete_by_id_and_notify_worker(Connection, ID, MetadataID) ->
    Result = apply(impl(db_impl_metadata), metadata_delete_by_id, [Connection, ID, MetadataID]),
    case Result of
        {ok, deleted} ->
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ -> EntityIDAtom ! {metadatadelete, MetadataID}, Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Automation commands that also notify workers
% ----------------------------------------------------------------------------------------------------
automation_set_and_notify_worker(Connection, GeobotID, AutomationID, Automation) ->
    Result = apply(impl(db_impl_automation), automation_setdetails, [Connection, GeobotID, AutomationID, Automation]),
    case Result of
        {ok, _AutomationID} ->
            GeobotIDAtom = binary_to_atom(GeobotID, utf8),
            case whereis(GeobotIDAtom) of
                undefined -> Result;
                _ ->
                    case imersia_db:automation_list(Connection, GeobotID) of
                        {ok, Automations} -> GeobotIDAtom ! {automationsrestart, Automations}, Result;
                        _ -> Result
                    end
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
automation_new_and_notify_worker(Connection, GeobotID, NewAutomation) ->
    Result = apply(impl(db_impl_automation), automation_new, [Connection, GeobotID, NewAutomation]),
    case Result of
        {ok, _AutomationID} ->
            GeobotIDAtom = binary_to_atom(GeobotID, utf8),
            case whereis(GeobotIDAtom) of
                undefined -> Result;
                _ ->
                    case imersia_db:automation_list(Connection, GeobotID) of
                        {ok, Automations} -> GeobotIDAtom ! {automationsrestart, Automations}, Result;
                        _ -> Result
                    end
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
automation_delete_and_notify_worker(Connection, GeobotID, AutomationID) ->
    Result = apply(impl(db_impl_automation), automation_delete, [Connection, GeobotID, AutomationID]),
    case Result of
        {ok, deleted} ->
            GeobotIDAtom = binary_to_atom(GeobotID, utf8),
            case whereis(GeobotIDAtom) of
                undefined -> Result;
                _ ->
                    case imersia_db:automation_list(Connection, GeobotID) of
                        {ok, Automations} -> GeobotIDAtom ! {automationsrestart, Automations}, Result;
                        _ -> Result
                    end
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Context commands that also notify workers
% ----------------------------------------------------------------------------------------------------
context_set_and_notify_worker(Connection, ID, ContextID, Attributes) ->
    Result = apply(impl(db_impl_context), context_set, [Connection, ID, ContextID, Attributes]),
    case Result of
        {ok, ContextID2} ->
            {ok, Context} = imersia_db:context_get(Connection, ID, ContextID2),
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ -> EntityIDAtom ! {context, Context}, Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
context_delete_and_notify_worker(Connection, ID, ContextID) ->
    Result = apply(impl(db_impl_context), context_delete, [Connection, ID, ContextID]),
    case Result of
        {ok, deleted} ->
            EntityIDAtom = binary_to_atom(ID, utf8),
            case whereis(EntityIDAtom) of
                undefined -> Result;
                _ -> EntityIDAtom ! {contextdelete, ContextID}, Result
            end;
        _ -> Result
    end.
% ----------------------------------------------------------------------------------------------------
% Given a set of context IDs, determine if the given Attribute is allowed.
% ----------------------------------------------------------------------------------------------------
check_context_is_allowed(_, _, undefined, _) -> false;
check_context_is_allowed(Connection, ID, ContextID, Attributes) when is_binary(ContextID) ->
    case imersia_db:context_get(Connection, ID, ContextID) of
        {ok, Context} ->
            AttributeSet = sets:from_list(Context#context.attributes),
            check_all_attributes(AttributeSet, Attributes);
        _ -> false
    end;
check_context_is_allowed(_, _, [], _) -> false;
check_context_is_allowed(Connection, ID, [ContextID | ContextIDs], Attributes) ->
    case imersia_db:context_get(Connection, ID, ContextID) of
        {ok, Context} ->
            AttributeSet = sets:from_list(Context#context.attributes),
            case check_all_attributes(AttributeSet, Attributes) of
                true -> true;
                false -> check_context_is_allowed(Connection, ID, ContextIDs, Attributes)
            end;
        _ -> check_context_is_allowed(Connection, ID, ContextIDs, Attributes)
    end.

% If only one attribute is given, test that, whereas if a list of attributes is given, test that they all are allowed.
check_all_attributes(AttributeSet, Attribute) when is_binary(Attribute) ->
    sets:is_element(Attribute, AttributeSet);
check_all_attributes(_, []) -> false;
check_all_attributes(AttributeSet, Attributes) ->
    check_non_empty_list_of_attributes(AttributeSet, Attributes).
check_non_empty_list_of_attributes(_, []) -> true;
check_non_empty_list_of_attributes(AttributeSet, [Attribute | Attributes]) ->
    sets:is_element(Attribute, AttributeSet) andalso check_non_empty_list_of_attributes(AttributeSet, Attributes).
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check that the user exists and matches given the SessionID and either the UserID or User Email.
% ----------------------------------------------------------------------------------------------------
user_exists_and_matches(_, undefined, _, _) -> {error, sessionid};
user_exists_and_matches(_, _, undefined, undefined) -> {error, userid};
user_exists_and_matches(Connection, SessionID, UserID, undefined) ->
    % Get the UserID from the SessionID
    case user_id_from_sessionid(Connection, SessionID) of
        % Check this matches the UserID parameter
        {ok, UserID} ->
            case user_get_details_by_userid(Connection, UserID) of
                {ok, _} -> {ok, matches};
                {error, Reason} -> {error, Reason}
            end;
        {ok, _} -> {error, userid};
        {error, Reason} -> {error, Reason}
    end;
user_exists_and_matches(Connection, SessionID, _, UserEmail) ->
    % Get the User ID from the Session
    case user_id_from_sessionid(Connection, SessionID) of
        {ok, UserID} ->
            % Check this matches the UserID from the Email
            case user_id(Connection, UserEmail) of
                {ok, UserID} -> {ok, matches};
                {ok, _} -> {error, useremail};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.
% ----------------------------------------------------------------------------------------------------
