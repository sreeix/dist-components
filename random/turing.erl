-module(turing).
-export([new/1, new/2, next/1, move/1]).

-include_lib("eunit/include/eunit.hrl").
-record(tape, {leftSymbols=[], currentSymbol=blankSymbol, rightSymbols=[] }).

-record(machine, {states=[],inputSymbols=[0,1],tapeSymbols=[0,1, blank], transitionFunction=undefined, startState=undefined, blankSymbol=blank, finalStates=undefined}).

new( _Machine =  {States, InputSymbols, TapeSymbols, TransitionFun, StartState, BlankSymbol, FinalStates}, _Tape = {LeftSymbols, CurrentSymbol, RightSymbols} ) ->
    %% validate that Input symbols are subset of tape symbols
    %% Start state is part of all states,
    %% blank symbol is part of tape symbols.
    %% valid final state exists taht is in the States.
    %% transition function is defined.
    io:format("Creating new of Turning machine ~p ~n", [_Tape]),
    Valid = is_sublist(InputSymbols, TapeSymbols)
        and is_sublist([BlankSymbol], TapeSymbols)
        and is_sublist([StartState], States)
        and (length(FinalStates) > 0) and is_sublist(FinalStates, States)
        and is_function(TransitionFun, 2),
    io:format("StatesL: InputSymbols: TapeSymbols ~p, ~p, ~p ~p ~p ~n", [is_sublist(InputSymbols, TapeSymbols),is_sublist(TapeSymbols, [StartState]), is_sublist(TapeSymbols, [BlankSymbol]), (length(FinalStates) > 0) and is_sublist(FinalStates, States), is_function(TransitionFun, 2)]),
        if
            Valid -> {#machine {states= States, inputSymbols= InputSymbols, tapeSymbols= TapeSymbols, transitionFunction= TransitionFun, startState= StartState, blankSymbol=BlankSymbol, finalStates= FinalStates}, StartState, #tape{leftSymbols=LeftSymbols, currentSymbol= CurrentSymbol, rightSymbols=RightSymbols}};
            true -> erlang:error(invalidMachine)
        end.


new (Machine) ->
    new(Machine, {[], blank, []}).

%% Add blank symbol if the next symbol to the right is empty. In a real turning machine, the tape is supposed to have blanks till infinity, but since we don't want to keep an infinite blank array(we are not hakell), we make it blank lazily.
next({Machine= #machine{}, CurrentState, Tape=#tape{}})  when Tape#tape.rightSymbols == [] ->
    next({Machine, CurrentState, Tape#tape{rightSymbols = [Machine#machine.blankSymbol]}});

%% Add blank symbol to the left as well if the left is empty
next({Machine = #machine{}, CurrentState, Tape= #tape{} })  when Tape#tape.leftSymbols == []->
    next({Machine, CurrentState, Tape#tape{leftSymbols= [Machine#machine.blankSymbol]}});

next({Machine = #machine{transitionFunction = TransitionFun}, CurrentState, Tape = #tape{leftSymbols = LeftSymbols, currentSymbol = CurrentSymbol, rightSymbols= [RightHead| RightTail]} }) ->
    {NewState, SymbolToWrite, Direction} = TransitionFun(CurrentState, CurrentSymbol),
    %% Validate the input symbol is the one in the Tape symbols
    %% validate the state is in the list of possible states.
    io:format("New State is ~p writing symbol [~p] to move [~p] ~n", [NewState, SymbolToWrite, Direction]),
    case contains(NewState, Machine#machine.states) of
        false -> throw("Transition Function output an state unknown in the TM definition");
        true -> nil %% keep moving.
    end,

    case contains(CurrentSymbol, Machine#machine.tapeSymbols) of
        false -> throw("Current Symbol is not in the Tape Symbol List");
        true -> nil %% doesn't matter i guess. Keep moving
    end,

    NewTape = case Direction of
                halt -> io:format("Halting the turing machine."),
                        Tape#tape{currentSymbol=SymbolToWrite}; %% Don't mess with the state of the machine, other than updating the new symbol to the curent position on the tape
                left -> io:format("Moving left as per the stucture and writing ~p ~p ~n", [SymbolToWrite, NewState]),
                        Tape#tape{leftSymbols = lists:droplast(Tape#tape.leftSymbols), currentSymbol = lists:last(Tape#tape.leftSymbols), rightSymbols = [SymbolToWrite | Tape#tape.rightSymbols]};
                  %% update current symbol, and move the head left which means that new symbol moves to the right,
                right-> io:format("Moving right as per the stucture and writing ~p ~p ~n", [SymbolToWrite, NewState]),
                        Tape#tape{leftSymbols = LeftSymbols ++ [SymbolToWrite], currentSymbol= RightHead , rightSymbols = RightTail} %% update current Symbole and move to the right
            end,
    {Machine, NewState, NewTape}.

contains(Item, InList) ->
    lists:any(fun (X) -> X == Item end, InList).

move(M) ->
    M2 = { Machine = #machine{}, NewState, Tape= #tape{}} = next(M),
    format_id(NewState, Tape),
    case contains(NewState, Machine#machine.finalStates) of
        true -> io:format("Final state reached. ~p ~n", [Tape#tape.leftSymbols]),
                M2;
        false -> move(M2)
    end.

format_id(State, Tape = #tape{}) ->
    io:format("~p~p~p~p~n", [Tape#tape.leftSymbols, State,Tape#tape.currentSymbol, Tape#tape.rightSymbols]).

is_sublist(SublistToTest, List)  ->
    lists:all(fun(Elem) -> lists:member(Elem, List) end, SublistToTest ).


machine_construction_test() ->
    %% no statesl
    ?assertError(invalidMachine, new( {[], [], [], undefined, blank, blank, []}, {[],[], []} )),
    %% undefined transition
    ?assertError(invalidMachine, new({[q1,q2], [], [], undefined, blank, blank, []}, {[],[], []})),
    _Machine = new({[q1,q2], [0,1], [0,1, blank], fun(_Z,_Y) -> 10 end, q1, blank, [q2]}, {[],[], []}).

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

    Machine1 = new( {[q1, q2], [0, 1], [0, 1, blank], TransitionFun, q1, blank, [q2]}, {[], blank, [1, 0, 1, 0, 1]} ),
%%    Machine1 = #machine{states = [q1, q2], transitionFunction=TransitionFun, startState=q1, finalStates=[q2] },
    {_Machine=#machine{}, CurrentState, Tape= #tape{}} = move(Machine1), %% Should halt.
    CurrentState  = q2,
    0 = Tape#tape.currentSymbol,
    io:format("The final State is ~p, and the tape has ~p ~n", [CurrentState, Tape#tape.leftSymbols]).
