
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([process_csv_file_with/2, process_csv_string_with/2]).
-export([process_csv_file_with/3, process_csv_string_with/3]).

process_csv_file_with(IoDevice, RowFunction) ->
    process_csv_file_with(IoDevice, RowFunction, []).

process_csv_string_with(String, RowFunction) ->
    process_csv_string_with(String, RowFunction, []).

process_csv_file_with(IoDevice, RowFunction, InitState) ->
    do_it(IoDevice, RowFunction, stream_from_file, InitState).

process_csv_string_with(String, RowFunction, InitState) ->
    do_it(String, RowFunction, stream_from_string, InitState).

%
% Internal API
%

do_it(Stream, RowFunction, StreamFunctionName, InitState) ->
    ProcessingPid = self(),
    ParsingPid = spawn(ecsv_parser, start_parsing, [ProcessingPid]),
    _ReadingPid = spawn(ecsv_reader, StreamFunctionName, [Stream, ParsingPid]),

    loop(RowFunction, InitState).

loop(RowFunction, State) ->
    receive
        {newline, [[]]} ->
            loop(RowFunction, State);
        {newline, NewLine} ->
            NewState = RowFunction(NewLine, State),
            loop(RowFunction, NewState);
        {done} ->
            {ok, State}
    end.
