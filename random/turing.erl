-module(turing).
-export([new/1, move/1]).

-include_lib("eunit/include/eunit.hrl").

new( Machine = {{_States, _InputSymbols, _TapeSymbols, _TransitionFun, _StartState, _FinalStates}, _ID = {[_LeftSymbols], _CurrentState, [_RightSymbols]} }) ->
    Machine.

move({Turing = {_States, _InputSymbols, _TapeSymbols, TransitionFun, _StartState, FinalStates}, ID = {[LeftSymbols], CurrentState, RightSymbols = [CurrentSymbol | RightSymbolsTail]} }) ->
    Transition = {NewState, SymbolToWrite, _Direction} = TransitionFun(CurrentState, CurrentSymbol),
    {NewState, SymbolToWrite, Direction} = case  lists:any(fun (X) -> X == NewState end, FinalStates) of
	true -> io:format("We have an accepting state: ~p ~n", [ID]),
		{NewState, SymbolToWrite, halt}; %% Halt now.
	false-> Transition
    end,
    NewId = case Direction of
		halt -> Transition;%% Not sure what to do
		left -> {lists:droplast(LeftSymbols), NewState, [SymbolToWrite | RightSymbols]};
		right-> {lists:append(LeftSymbols,[SymbolToWrite]), NewState, RightSymbolsTail}

	    end,
    {Turing, NewId}.




move_test() ->
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

    Machine1 = new({ {[q1, q2], [0, 1], [0, 1, blank], TransitionFun, q1, blank, [q2]}, {[], q1, [1, 0, 0]} }),
    move(Machine1).
