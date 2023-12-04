% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: File storage implementation using simple local file structure
% ----------------------------------------------------------------------------------------------------
%% @hidden

-module(imersia_local_files).

-include("imersia_files.hrl").

-export([init/0, upload/4, download/2, downloadurl/1, process_image/4, delete_file/2, file_exists/2, delete_all_files/1, list_all_files/1, write_to_logfile/2]).


% ----------------------------------------------------------------------------------------------------
% Any initialisation that might be required
% ----------------------------------------------------------------------------------------------------
init() -> ok.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Load a file to an entity from the API
% ----------------------------------------------------------------------------------------------------
upload(undefined, Filename, _MimeType, FileData) ->
    % Create a temporary directory for the file
    LocalFile = imersia_misc:basedir() ++ "/files/temp/" ++ binary_to_list(Filename),
    FileURL = << "/files/temp/", Filename/binary >>,
    do_upload(LocalFile, FileURL, FileData);

upload(ID, Filename, _MimeType, FileData) ->
    % Check there is a directory for the ID - if not, create it
    LocalFile = imersia_misc:basedir() ++ "/files/" ++ binary_to_list(ID) ++ "/" ++ binary_to_list(Filename),
    FileURL = << "/files/", ID/binary, "/", Filename/binary >>,
    do_upload(LocalFile, FileURL, FileData).

do_upload(LocalFile, FileURL, FileData) ->
    case filelib:ensure_dir(LocalFile) of
        ok ->
            % Open the local file
            case file:open(LocalFile, [read, write]) of
                {ok, IoDevice} ->
                    % Save the file
                    case file:write(IoDevice, FileData) of
                        ok -> file:close(IoDevice), {ok, FileURL};
                        _ -> file:close(IoDevice), {error, writing}
                    end;
                _ -> {error, filename}
            end;
        _ -> {error, filename}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Given an ID and Filename, return binary filedata or an appropriate error
% ----------------------------------------------------------------------------------------------------
download(undefined, _) -> {error, undefined, reading};
download(_, undefined) -> {error, undefined, reading};
download(ID, Filename) ->
    % Construct the local filename
    LocalFilename = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID) ++ "/" ++ erlang:binary_to_list(Filename),

    % Send a blank image if the file referenced doesn't exist
    ActualFilename = case file:read_file_info(LocalFilename) of
        {ok, _} -> LocalFilename;
        _ -> code:priv_dir(mrserver) ++ "/noimage.png"
    end,

    % Get the MIME type
    Mimetype = mimerl:filename(list_to_binary(ActualFilename)),

    % Respond appropriately depending on whether the file exists or not.
    case file:read_file(ActualFilename) of
        {ok, FileData} -> {ok, Mimetype, FileData};
        _ -> {error, undefined, reading}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Download an image from another URL (ie cache it locally)
% TODO: Use a reference to this server apart from localhost
% ----------------------------------------------------------------------------------------------------
downloadurl(undefined) ->  %downloadurl(<<"/images/noimage.png">>);
    ActualFilename = code:priv_dir(mrserver) ++ "/noimage.png",
    % Get the MIME type
    Mimetype = mimerl:filename(list_to_binary(ActualFilename)),

    % Respond appropriately depending on whether the file exists or not.
    case file:read_file(ActualFilename) of
        {ok, FileData} -> {ok, Mimetype, FileData};
        _ -> {error, undefined, reading}
    end;

downloadurl(URL) ->
    % Either it's a link to a file on this server, or somewhere else
    URL0 = case string:str(binary_to_list(URL), "http") of
        0 -> << "https://localhost", URL/binary >>;
        1 -> URL
    end,

    % Create a unique filename for this file by removing https and http and slashes
    URL1 = binary:replace(URL0, <<"https://">>, <<"">>, [global]),
    URL2 = binary:replace(URL1, <<"http://">>, <<"">>, [global]),
    URL3 = binary:replace(URL2, <<"/">>, <<"_">>, [global]),
    Filename = imersia_misc:basedir() ++ "/files/temp/" ++ erlang:binary_to_list(URL3),

    % Go get the file and stream it into this file system
    case filelib:ensure_dir(Filename) of
        ok ->
            % Get the mime type
            Mimetype = mimerl:filename(erlang:list_to_binary(string:to_lower(Filename))),

            % Get the file
            case httpc:request(get, {erlang:binary_to_list(URL0), []}, [], [{sync, true}, {stream, Filename}]) of
                {ok, _Result} ->
                    case file:read_file(Filename) of
                        {ok, FileData} -> {ok, Mimetype, FileData};
                        _ -> {error, undefined, reading}
                    end;
                {error, _Message} ->
                    file:delete(Filename), {error, undefined, reading} % Deleting the file in case it was partially downloaded or blank / black
            end;
        _ -> {error, undefined, reading}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
write_to_logfile(ID, LineOfText) ->
    LocalFile = imersia_misc:basedir() ++ "/files/" ++ binary_to_list(ID) ++ "/logfile.txt",
    FileURL = << "/files/", ID/binary, "/logfile.txt" >>,
    case filelib:ensure_dir(LocalFile) of
        ok ->
            % Open the local file
            case file:open(LocalFile, [append]) of
                {ok, IoDevice} ->
                    % Save the file
                    case file:write(IoDevice, LineOfText) of
                        ok -> file:close(IoDevice), {ok, FileURL};
                        Reason -> erlang:display(Reason), file:close(IoDevice), {error, writing}
                    end;
                _ -> {error, filename}
            end;
        _ -> {error, filename}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Process an image file according to various parameters
% Requires ImageMagick to be installed on the OS
% ----------------------------------------------------------------------------------------------------
process_image(FileData, Pixels, <<"round">>, Format) -> process_image(FileData, Pixels, <<"circle">>, Format);
process_image(FileData, Pixels, <<"circle">>, Format) ->
	PixelsDiv2 = trunc(Pixels / 2),

    % Resize image
	{ok, ProcessedFile1} = emagick:with(FileData, Format, [
		fun emagick:with_imageinfo/1,
		fun({Args, _Info}) ->
			Opts = [
				{resize, iolist_to_binary(io_lib:fwrite("~Bx~B^", [Pixels, Pixels]))},
				{extent, iolist_to_binary(io_lib:fwrite("~Bx~B", [Pixels, Pixels]))}
			],
			{_, Converted} = emagick:with_convert(Args, Format, Opts),
			{ok, Converted}
		end]),
    % Make it round
	{ok, ProcessedFile} = emagick:with(ProcessedFile1, Format, [
		fun emagick:with_imageinfo/1,
		fun({Args, _Info}) ->
			{Tempfile, _} = Args,
			Opts = [
				{size, iolist_to_binary(io_lib:fwrite("~Bx~B xc:none", [Pixels, Pixels]))},
				{fill, Tempfile},
				{draw, iolist_to_binary(io_lib:fwrite("\"ellipse ~B,~B ~B,~B 0, 360\"", [PixelsDiv2, PixelsDiv2, PixelsDiv2, PixelsDiv2]))}
			],
			{_, Converted} = emagick:with_convert(Args, Format, Opts),
			{ok, Converted}
		end]),
	ProcessedFile;

process_image(FileData, Pixels, <<"square">>, Format) ->
	{ok, ProcessedFile} = emagick:with(FileData, Format, [
		fun emagick:with_imageinfo/1,
		fun({Args, _Info}) ->
			Opts = [
				{resize, iolist_to_binary(io_lib:fwrite("~Bx~B^", [Pixels, Pixels]))},
				{extent, iolist_to_binary(io_lib:fwrite("~Bx~B", [Pixels, Pixels]))}
			],
			{_, Converted} = emagick:with_convert(Args, Format, Opts),
			{ok, Converted}
		end]),
	ProcessedFile;

process_image(FileData, Pixels, _, Format) ->
	{ok, ProcessedFile} = emagick:with(FileData, Format, [
		fun emagick:with_imageinfo/1,
		fun({Args, _Info}) ->
			Opts = [
				{resize, iolist_to_binary(io_lib:fwrite("~Bx~B^", [Pixels, Pixels]))}
			],
			{_, Converted} = emagick:with_convert(Args, Format, Opts),
			{ok, Converted}
		end]),
	ProcessedFile.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Check if a file exists or not
% ----------------------------------------------------------------------------------------------------
file_exists(ID, Filename) ->
    LocalFilename = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID) ++ "/" ++ erlang:binary_to_list(Filename),
    case file:read_file_info(LocalFilename) of
        {ok, _} -> {ok, exists};
        _ -> {error, file}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete a file for a given entity
% ----------------------------------------------------------------------------------------------------
delete_file(ID, Filename) ->
    LocalFilename = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID) ++ "/" ++ erlang:binary_to_list(Filename),
    case file:delete(LocalFilename) of
        ok -> {ok, deleted};
        {error, enoent} -> {ok, deleted};
        _ -> {error, file}
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% Delete all the files and folder for the given entity
% ----------------------------------------------------------------------------------------------------
delete_all_files(ID) ->
    LocalDirName = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID),
    case file:list_dir_all(LocalDirName) of
        {ok, Files} ->
            case delete_files(ID, Files) of
                ok ->
                    case file:del_dir(LocalDirName) of
                        ok -> {ok, deleted};
                        _ -> {error, file}
                    end;
                _ -> {error, file}
            end;
        {error, enoent} -> {ok, deleted};
        _ -> {error, file}
    end.

delete_files(_, []) -> ok;
delete_files(ID, [Filename | Files]) ->
    LocalFilename = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID) ++ "/" ++ Filename,
    case file:delete(LocalFilename) of
        ok -> delete_files(ID, Files);
        {error, enoent} -> delete_files(ID, Files);
        _ -> error
    end.
% ----------------------------------------------------------------------------------------------------



% ----------------------------------------------------------------------------------------------------
% List all the files for the given ID
% ----------------------------------------------------------------------------------------------------
list_all_files(ID) ->
    LocalDirName = imersia_misc:basedir() ++ "/files/" ++ erlang:binary_to_list(ID),
    case file:list_dir_all(LocalDirName) of
        {ok, Files} -> {ok, process_filenames(ID, Files)};
        _ -> {error, id}
    end.

process_filenames(_, []) -> [];
process_filenames(ID, [Filename | Rest]) ->
    FilenameBinary = erlang:list_to_binary(Filename),
    [<< "/files/", ID/binary, "/",  FilenameBinary/binary >> | process_filenames(ID, Rest)].
% ----------------------------------------------------------------------------------------------------
