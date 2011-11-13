
% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv_reader).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([parse_from_io/2, parse_from_string/2]).

%% @doc parse using an io device such as file as the string source
parse_from_io(IoDevice, ResultPid) ->
    IoDeviceIterator = fun(Io) ->
        {io:get_chars(Io, "", 1), Io}
    end,
    iterate_chars(spawn(ecsv_parser, start_parsing, [ResultPid]), IoDeviceIterator, IoDevice).

%% @doc parse csv from a string
parse_from_string(String, ResultPid) ->
    StringIterator = fun(StringList) ->
        get_first_char(StringList)
    end,
    iterate_chars(spawn(ecsv_parser, start_parsing, [ResultPid]), StringIterator, String).

%%
%% Local Functions
%%

iterate_chars(ParserPid, IteratorFun, TextSource) ->
    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(ParserPid, IteratorFun, UpdatedTextSource, FirstChar).

iterate_chars(Pid, _, _, eof) ->
    Pid ! {eof},
    ok;

iterate_chars(Pid, IteratorFun, TextSource, Char) ->
    Pid ! {char, clean_char_argument(Char)},

    {FirstChar, UpdatedTextSource} = IteratorFun(TextSource),

    iterate_chars(Pid, IteratorFun, UpdatedTextSource, FirstChar).

%% @doc make sure that an integer denoting a char is returned instead of a string
clean_char_argument([CharInt | _]) ->
    CharInt;
clean_char_argument(CharInt) when is_integer(CharInt) ->
    CharInt.

%% @doc returns tuple {FirstChar, RemainingChars} or {eof, []} if no more chars
%% remains
get_first_char([]) ->
    {eof, []};
get_first_char([FirstChar | Tail]) ->
    {FirstChar, Tail}.
