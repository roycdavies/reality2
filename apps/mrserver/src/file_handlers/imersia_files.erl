% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: File storage implementation using
% ----------------------------------------------------------------------------------------------------


-module(imersia_files).

-include("imersia_files.hrl").

-export([init/0, upload/4, download/2, downloadurl/1, process_image/4, file_exists/2, delete_file/2, delete_all_files/1, list_all_files/1, write_to_logfile/2]).


impl(Sub)                                                   -> {ok, Module} = application:get_env(mrserver, Sub), Module.
init()                                                      -> set_settings(), apply(impl(file_impl), init, []).
upload(ID, Filename, MimeType, FileData)                    -> apply(impl(file_impl), upload, [ID, Filename, MimeType, FileData]).
process_image(FileData, Pixels, Shape, Format)              -> apply(impl(file_impl), process_image, [FileData, Pixels, Shape, Format]).
download(ID, Filename)                                      -> apply(impl(file_impl), download, [ID, Filename]).
downloadurl(URL)                                            -> apply(impl(file_impl), downloadurl, [URL]).
file_exists(ID, Filename)                                   -> apply(impl(file_impl), file_exists, [ID, Filename]).
delete_file(ID, Filename)                                   -> apply(impl(file_impl), delete_file, [ID, Filename]).
delete_all_files(ID)                                        -> apply(impl(file_impl), delete_all_files, [ID]).
list_all_files(ID)                                          -> apply(impl(file_impl), list_all_files, [ID]).
write_to_logfile(ID, LineOfText)                            -> apply(impl(file_impl), write_to_logfile, [ID, LineOfText]).

% ----------------------------------------------------------------------------------------------------
% Set up environment variables to define the current files implementation
% ----------------------------------------------------------------------------------------------------
set_settings() ->
    FileImplementation = erlang:binary_to_list(imersia_settings:get_setting(file, implementation)),
    application:set_env(mrserver, file_impl, list_to_atom("imersia_" ++ FileImplementation ++ "_files"), [{persistent, true}]).
% ----------------------------------------------------------------------------------------------------
