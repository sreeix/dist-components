-module(turing).
-export([new/1, next/1, move/1]).

-include_lib("eunit/include/eunit.hrl").
-record(tape, {leftSymbols=[], currentSymbol=blankSymbol, rightSymbols=[] }).

-record(machine, {states=[],inputSymbols=[0,1],tapeSymbols=[0,1, blank], transitionFunction=undefined, startState=undefined, blankSymbol=blank, finalStates=undefined, tape=undefined}).

new( _Machine = { {States, InputSymbols, TapeSymbols, TransitionFun, StartState, BlankSymbol, FinalStates}, _ID = {LeftSymbols, CurrentSymbol, RightSymbols} }) ->
    %% validate that Input symbols are subset of tape symbols
    %% Start state is part of all states,
    %% blank symbol is part of tape symbols.
    %% valid final state exists taht is in the States.
    %% transition function is defined.
    Valid = is_sublist(InputSymbols, TapeSymbols)
        and is_sublist([BlankSymbol], TapeSymbols)
        and is_sublist([StartState], States)
        and (length(FinalStates) > 0) and is_sublist(FinalStates, States)
        and is_function(TransitionFun, 2),
    io:format("StatesL: InputSymbols: TapeSymbols ~p, ~p, ~p ~p ~p ~n", [is_sublist(InputSymbols, TapeSymbols),is_sublist(TapeSymbols, [StartState]), is_sublist(TapeSymbols, [BlankSymbol]), (length(FinalStates) > 0) and is_sublist(FinalStates, States),is_function(TransitionFun, 2)]),
        if
            Valid -> #machine {states= States, inputSymbols= InputSymbols, tapeSymbols= TapeSymbols, transitionFunction= TransitionFun, startState= StartState, blankSymbol=BlankSymbol, finalStates= FinalStates, tape= #tape{leftSymbols=LeftSymbols, currentSymbol= CurrentSymbol, rightSymbols=RightSymbols}};
            true -> erlang:error(invalidMachine)
        end.

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
        true -> 42 %% doesn't matter i guess.
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

is_sublist(SublistToTest, List)  ->
    lists:all(fun(Elem) -> lists:member(Elem, List) end, SublistToTest ).


machine_construction_test() ->
    %% no statesl
    ?assertError(invalidMachine, new({{[], [], [], undefined, blank, blank, []}, {[],[], []}})),
    %% undefined transition
    ?assertError(invalidMachine, new({{[q1,q2], [], [], undefined, blank, blank, []}, {[],[], []}})),
    _Machine = new({{[q1,q2], [0,1], [0,1, blank], fun(_Z,_Y) -> 10 end, q1, blank, [q2]}, {[],[], []}}).

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
%%    Machine1 = #machine{states = [q1, q2], transitionFunction=TransitionFun, startState=q1, finalStates=[q2] },
    move(Machine1). %% Should halt.
