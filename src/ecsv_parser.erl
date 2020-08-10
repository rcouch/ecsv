% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv_parser).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-include("ecsv.hrl").

%
% This module is a raw CSV parser.
%
% This parser is based on the blog post written by Andy Till located
% here http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html.
%
% This parser supports well formed csv files which are
% - a set of lines ending with a \n
% - each line contains a set of fields separated with a comma (,)
% - each field value can be enclosed with double quote (") ONLY
% - each field value can be empty
%
% Please note:
% - This parser has no failsafe mechanism if the file is badly formed!
%   But the line a,,,,,\n is perfectly fine.
% - This parser doesn't allow a return (\n) in a field value!
%

-export([init/1, init/2, parse_with_character/2, end_parsing/1]).

-record(pstate, {
    state, % ready, in_quotes, skip_to_delimiter, eof
    current_line,
    current_value,
    opts,
    process_fun,
    process_fun_state
}).

% 4 states:
%   ready
%   in_quotes
%   skip_to_delimiter
%   eof

init(ProcessingFun) ->
    init(ProcessingFun, []).

init(ProcessingFun, ProcessingFunInitState) ->
    init(#ecsv_opts{}, ProcessingFun, ProcessingFunInitState).

init(Opts, ProcessingFun, ProcessingFunInitState) ->
    #pstate{
        state = ready,
        current_line = [],
        current_value = [],
        opts = Opts,
        process_fun = ProcessingFun,
        process_fun_state = ProcessingFunInitState
    }.

parse_with_character(Character, PState) when is_integer(Character) ->
    parse_with({char, Character}, PState).

end_parsing(PState) ->
    FinalState = parse_with({eof}, PState),

    {ok, FinalState#pstate.process_fun_state}.

% -----------------------------------------------------------------------------

parse_with(Input, #pstate{state=State}=PState) ->
    case State of
        ready ->
            do_ready(Input, PState);
        in_quotes ->
            do_in_quotes(Input, PState);
        skip_to_delimiter ->
            do_skip_to_delimiter(Input, PState);
        eof ->
            PState;
        _ ->
            throw({error, wrong_state})
    end.

do_ready(
    Input,
    #pstate{
        opts=Opts,
        current_line=CurrentLine,
        current_value=CurrentValue,
        process_fun=ProcessingFun,
        process_fun_state=ProcessingFunState
    }=PState
    ) ->
    Delimiter = Opts#ecsv_opts.delimiter,
    case Input of
        {eof} ->
            NewLine = case CurrentValue of
                [] -> lists:reverse(CurrentLine);
                _  -> lists:reverse([lists:reverse(CurrentValue) | CurrentLine])
            end,
            UpdatedProcessingFunState =
                process_new_line(ProcessingFun, NewLine, ProcessingFunState),
            UpdatedProcessingFunState1 =
                ProcessingFun({eof}, UpdatedProcessingFunState),
            PState#pstate{
                state=eof,
                current_line=[],
                current_value=[],
                process_fun_state=UpdatedProcessingFunState1
            };
        {char, Char} when (Char == $") ->
            % pass an empty string to in_quotes as we do not want the
            % preceeding characters to be included, only those in quotes
            PState#pstate{state=in_quotes, current_value=[]};
        {char, Char} when Char == Delimiter ->
            PState#pstate{
                current_line=[lists:reverse(CurrentValue) | CurrentLine],
                current_value=[]
            };
        {char, Char} when Char == $\n ->
            % a new line has been parsed: time to send it back
            NewLine = lists:reverse([lists:reverse(CurrentValue) | CurrentLine]),
            UpdatedProcessingFunState =
                process_new_line(ProcessingFun, NewLine, ProcessingFunState),
            PState#pstate{
                current_line=[],
                current_value=[],
                process_fun_state=UpdatedProcessingFunState
            };
        {char, Char} when Char == $\r ->
            % ignore line feed characters
            PState;
        {char, Char} ->
            PState#pstate{current_value=[Char | CurrentValue]}
    end.

do_in_quotes(Input, #pstate{current_value = CurrentValue} = PState) ->
    case Input of
        {eof} -> on_eof(on_new_line(PState));
        {char, Char} when Char == $" ->
            case CurrentValue of
		[ $\\ | TCurrentValue] ->
                    %% Handle the case when there is an escaped double quote in the field
                    io:format("do_in_quotes:2: ~p~n", [ lists:reverse([Char | TCurrentValue]) ]),
                    PState#pstate{current_value=[Char | TCurrentValue]};
                _ ->
                    io:format("do_in_quotes:3: ~p~n", [ lists:reverse(CurrentValue) ]),
                    PState#pstate{
              		state=skip_to_delimiter,
              		current_value=CurrentValue
             	    }
	   end;
        {char, Char} when Char == $\\ ->
	    case CurrentValue of
		[ $\\ | _] -> PState;
                _ -> PState#pstate{current_value=[Char | CurrentValue]}
	    end;
        {char, Char} ->
            io:format("do_in_quotes:1: ~p~n", [ lists:reverse([Char | CurrentValue]) ]),
            PState#pstate{current_value=[Char | CurrentValue]}
    end.

do_skip_to_delimiter(Input, #pstate{
                                opts                = Opts,
                                current_line        = CurrentLine,
                                current_value       = CurrentValue
                            } = PState) ->
    case Input of
        {eof} -> on_eof(on_new_line(PState));
        {char, Char} when (Char == $\n) -> on_new_line(PState);
        {char, Char} when (Char == Opts#ecsv_opts.delimiter) ->
            PState#pstate{
                state           = ready,
                current_value   = [],
                current_line    = [lists:reverse(CurrentValue) | CurrentLine]
            };
        {char, Char} ->
            io:format("do_skip_to_delimiter: ~s~n", [ lists:reverse([Char | CurrentValue]) ]),
            PState#pstate{
                state           = in_quotes,
                current_value   = [Char | CurrentValue]
            }
    end.

on_new_line(#pstate{
                        current_line        = CurrentLine,
                        current_value       = CurrentValue,
                        process_fun         = ProcessingFun,
                        process_fun_state   = ProcessingFunState
                    } = PState) ->
    NewLine = lists:reverse([lists:reverse(CurrentValue) | CurrentLine]),
    UpdatedProcessingFunState =
        process_new_line(ProcessingFun, NewLine, ProcessingFunState),
    PState#pstate{
        state               = ready,
        current_line        = [],
        current_value       = [],
        process_fun_state   = UpdatedProcessingFunState
    }.

on_eof(#pstate{
                    process_fun         = ProcessingFun,
                    process_fun_state   = ProcessingFunState
              } = PState) ->
    UpdatedProcessingFunState = ProcessingFun({eof}, ProcessingFunState),
    PState#pstate{
        state               = eof,
        current_line        = [],
        current_value       = [],
        process_fun_state   = UpdatedProcessingFunState
    }.

process_new_line(_ProcessingFun, [], State) ->
    % ignore empty lines
    State;
process_new_line(ProcessingFun, NewLine, State) ->
    ProcessingFun({newline, NewLine}, State).
