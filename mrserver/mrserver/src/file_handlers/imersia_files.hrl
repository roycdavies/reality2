% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: File Storage abstract data structure functions that have to be implemented
% ----------------------------------------------------------------------------------------------------


% ----------------------------------------------------------------------------------------------------
% Initialise the file management subsystem
% ----------------------------------------------------------------------------------------------------
-spec init() ->
    ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Upload and save a file for a given entity (Companion, Channel, Geobot)
% ----------------------------------------------------------------------------------------------------
-spec upload(ID :: binary(), Filename :: binary(), MimeType :: binary(), FileData :: binary()) ->
    {ok, Filename :: binary()} |    % Resulting URL filename
    {error, writing} |              % Error writing the data to the file
    {error, filename}.              % An Error occurred in loading the file
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given an image in some format, process it using ImageMagick into round or square, and resize it
% to thumb, small, medium or large size.
% ----------------------------------------------------------------------------------------------------
-spec process_image(FileData :: binary(), Pixels :: integer(), Shape :: binary(), Format :: atom()) ->
    binary().                       % Resulting processed image
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given an ID and Filename, return binary filedata or an appropriate error
% ----------------------------------------------------------------------------------------------------
-spec download(ID :: binary(), Filename :: binary()) ->
    {ok, binary(), binary()} |      % Mimetype, FileData
    {error, undefined, reading}.    % Error reading the file
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Download and cache an image from another URL
% ----------------------------------------------------------------------------------------------------
-spec downloadurl(URL :: binary()) ->
    {ok, binary(), binary()} |      % Mimetype, FileData
    {error, undefined, reading}.    % Error reading the file
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Specify whether a file exists or not
% ----------------------------------------------------------------------------------------------------
-spec file_exists(ID :: binary(), Filename :: binary()) ->
    {ok, exists} |              % All OK, file exists
    {error, file}.              % File doesn't exist
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete the given file
% ----------------------------------------------------------------------------------------------------
-spec delete_file(ID :: binary(), Filename :: binary()) ->
    {ok, deleted} |             % OK, file deleted
    {error, file}.              % Error deleting file
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the files (and the directory) for the given ID
% ----------------------------------------------------------------------------------------------------
-spec delete_all_files(ID :: binary()) ->
    {ok, deleted} |             % OK, all files deleted
    {error, file}.              % Error deleting files
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the files for the given ID
% ----------------------------------------------------------------------------------------------------
-spec list_all_files(ID :: binary()) ->
    {ok, Filenames  :: list(binary())} |    % Return a list of filenames
    {error, file}.                          % Error getting filenames
% ----------------------------------------------------------------------------------------------------



-spec write_to_logfile(ID :: binary(), LineOfText :: binary()) ->
    {ok, Filename :: binary()} |    % Resulting URL filename (logfile.txt)
    {error, writing} |              % Error writing the data to the file
    {error, filename}.              % An Error occurred in loading the file
