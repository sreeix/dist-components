-module(turing).
-export([new/1, next/1, move/1]).

-include_lib("eunit/include/eunit.hrl").

new( Machine = {{_States, _InputSymbols, _TapeSymbols, _TransitionFun, _StartState, _BlankSymbol, _FinalStates}, _ID = {_LeftSymbols, _CurrentState, _RightSymbols} }) ->
    %% validated that Input symbols are subset of tape symbols
    Machine.

next({Turing = {_States, _InputSymbols, _TapeSymbols, _TransitionFun, _StartState, BlankSymbol, _FinalStates}, {LeftSymbols, CurrentState, []} }) ->
    next({Turing, {LeftSymbols, CurrentState, [BlankSymbol]}});

next({Turing = {_States, _InputSymbols, _TapeSymbols, _TransitionFun, _StartState, BlankSymbol, _FinalStates}, {[], CurrentState, RightSymbols} }) ->
    next({Turing, {[BlankSymbol],CurrentState, RightSymbols}});

next({Turing = {States, _InputSymbols, TapeSymbols, TransitionFun, _StartState,_BlankSymbol, _FinalStates}, {LeftSymbols, CurrentState, RightSymbols = [CurrentSymbol | RightSymbolsTail]} }) ->
    {NewState, SymbolToWrite, Direction} = TransitionFun(CurrentState, CurrentSymbol),
    %% Validate the input symbol is the one in the Tape symbols
    %% validate the state is in the list of possible states.
    io:format("New State is ~p writing symbol [~p] to move [~p] ~n", [NewState, SymbolToWrite, Direction]),
    case contains(NewState, States) of
	false -> throw("Transition Function output an state unknown in the TM definition");
	true -> 10
    end,

    case contains(CurrentSymbol, TapeSymbols) of
	false -> throw("Current Symbol is not in the Tape Symbol List");
	true -> 42
    end,

    NewId = case Direction of
		halt -> io:format("Halting the turing machine."),
			{LeftSymbols, NewState, RightSymbols};
		left -> io:format("Moving left as per the stucture and writing ~p ~p ~n", [SymbolToWrite, NewState]),
			{lists:droplast(LeftSymbols), NewState, [SymbolToWrite | RightSymbols]};
		right-> io:format("Moving right as per the stucture and writing ~p ~p ~n", [SymbolToWrite, NewState]),
			{lists:append(LeftSymbols,[SymbolToWrite]), NewState, RightSymbolsTail}
	    end,
    {Turing, NewId}.

contains(Item, InList) ->
    lists:any(fun (X) -> X == Item end, InList).

move(M) ->
    M2 = { {_, _, _, _, _, _, FinalStates}, Id = {LeftSymbols, NewState, _}} = next(M),
    format_id(Id),
    case contains(NewState, FinalStates) of
	true -> io:format("Final state reached. ~p ~n", [[LeftSymbols]]);
	false -> move(M2)
    end.

format_id({Left, State, Right}) ->
    io:format("~p~p~p~n", [Left, State, Right]).

move_to_right_test() ->
    %% Simple machine to set all bits to 0, if 1 it will set the bit to 0 and move right. Till it sees blank, if it sees blank it will enter accepting state and halt.
    TransitionFun = fun (State, TapeSymbol) ->
			    case TapeSymbol of
				blank -> {q2, 0, halt};
				_ -> {State, 0, right}
			    end
		    end,
    %% q1 q2 are the states in the system
    %% 0, 1 are the input symbols that will form the language of tm
    %% 0, 1, b are the symbols that can be on the tape.
    %% q1 is the start state
    %% q2 is the accepting state
    %% b is the blank symbol represented on the tape

    Machine1 = new({ {[q1, q2], [0, 1], [0, 1, blank], TransitionFun, q1, blank, [q2]}, {[], q1, [1, 0, 1, 0, 1]} }),
    move(Machine1). %% Should halt.
