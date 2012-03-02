
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([process_csv_file_with/2, process_csv_string_with/2]).
-export([process_csv_file_with/3, process_csv_string_with/3]).

%% @doc parse a csv file and process each parsed row with the RowFunction
process_csv_file_with(IoDevice, RowFunction) ->
    process_csv_file_with(IoDevice, RowFunction, []).

%% @doc parse a csv string and process each parsed row with the RowFunction
process_csv_string_with(String, RowFunction) ->
    process_csv_string_with(String, RowFunction, []).

%% @doc parse a csv file and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_file_with(IoDevice, RowFunction, RowFunctionInitState) ->
    InitState = ecsv_parser:init(RowFunction, RowFunctionInitState),
    stream_from_file(IoDevice, InitState).

%% @doc parse a csv string and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_string_with(String, RowFunction, RowFunctionInitState) ->
    InitState = ecsv_parser:init(RowFunction, RowFunctionInitState),
    stream_from_string(String, InitState).

% -----------------------------------------------------------------------------

stream_from_string(String, InitState) ->
    StringIterator = fun(StringList) ->
        get_first_char(StringList)
    end,
    iterate_chars(StringIterator, String, InitState).

stream_from_file(IoDevice, InitState) ->
    IoDeviceIterator = fun(Io) ->
        {io:get_chars(Io, "", 1), Io}
    end,
    iterate_chars(IoDeviceIterator, IoDevice, InitState).

iterate_chars(IteratorFun, TextSource, State) ->
    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(IteratorFun, UpdatedTextSource, State, FirstChar).

iterate_chars(_, _, State, eof) ->
    ecsv_parser:end_parsing(State);
iterate_chars(IteratorFun, TextSource, State, Char) ->
    UpdatedState = ecsv_parser:parse_with_character(clean_char_argument(Char), State),

    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(IteratorFun, UpdatedTextSource, UpdatedState, FirstChar).

%% @doc make sure that an integer denoting a char is returned instead of a string
clean_char_argument([CharInt | _]) ->
    CharInt;
clean_char_argument(CharInt) when is_integer(CharInt) ->
    CharInt.

get_first_char([]) ->
    {eof, []};
get_first_char([FirstChar | Tail]) ->
    {FirstChar, Tail}.
