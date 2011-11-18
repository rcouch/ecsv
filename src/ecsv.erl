
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([process_csv_file_with/2, process_csv_string_with/2]).

process_csv_file_with(IoDevice, RowFunction) ->
    ProcessingPid = self(),
    ParsingPid = spawn(ecsv_parser, start_parsing, [ProcessingPid]),
    _ReadingPid = spawn(ecsv_reader, stream_from_file, [IoDevice, ParsingPid]),

    loop(RowFunction),
    ok.

process_csv_string_with(String, RowFunction) ->
    ProcessingPid = self(),
    ParsingPid = spawn(ecsv_parser, start_parsing, [ProcessingPid]),
    _ReadingPid = spawn(ecsv_reader, stream_from_string, [String, ParsingPid]),

    loop(RowFunction),
    ok.

loop(RowFunction) ->
    receive
        {newline, [[]]} ->
            loop(RowFunction);
        {newline, NewLine} ->
            RowFunction(NewLine),
            loop(RowFunction);
        {done} ->
            ok
    end.
