-module(sorts).
-export([selection/1, insertion/1]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


selection([]) -> [];
selection(List) ->
    Min = lists:min(List),
    [ Min | selection(lists:delete(Min, List))].



insert(X, []) ->
    [X];
insert(X, List = [H | T]) ->
    case (X > H) of
	true -> [H | insert(X, T)];
	false -> [X | List]
    end.
insertion([], Acc) ->
    Acc;
insertion([H | T], A) ->
    insertion(T, insert(H, A)).

insertion(List) ->
    insertion(List, []).


%% insertion_h(H, List) ->
%%     [ || X <- lists:seq(1, H-1)]

%% hsort_seq(Generator, Start, End) ->
%%     case Start < End of
%%	true ->     [Start | hsort_seq(Generator, Generator(Start), End)];
%%	false  ->[]
%%     end.



%% shell(List)->
%%     [ insertion_h(X, List) || X <- hsort_seq(fun(X) -> (3 * X)  + 1 end, 1, length(List))].

msort([X | []]) -> [X];
msort([X |[ Y | [] ]]) ->
    case X > Y of
	true -> [Y | [X]];
	false -> [X | [Y]]
    end;

msort(List) -> merge(List).

merge([], L2)-> L2;
merge(L1, []) -> L1;

merge(List1 = [H1 | T1], List2 = [H2 | T2]) ->
    case H1 > H2 of
	true ->
	    [H2 | merge(List1, T2)];
	false  -> [H1 | merge(T1, List2)]
    end.

merge([]) -> [];
merge(List) ->
    {FirstHalf, SecondHalf} = lists:split(length(List) div 2, List),
    merge(msort(FirstHalf), msort(SecondHalf)).


shuffle(List) ->
    [ V ||{_Random, V} <- lists:keysort(1, [{random:uniform(), X} || X <- List])].


quick_int([Pivot| T]) ->
    quick([X || X <- T, X< Pivot]) ++ [Pivot] ++ quick([X || X <- T, X> Pivot]).
quick([]) ->
    [];
quick(List) ->
    shuffle(List),
    quick_int(List).



sort_test() ->
    [3, 5, 6] = sorts:selection([5, 6, 3]),
    [] = sorts:selection([]),
    [3, 5, 6] = sorts:insertion([5, 6, 3]),
    [] = sorts:insertion([]),
    [3, 5, 6] = sorts:merge([5, 6, 3]),
    [] = sorts:merge([]),
    [1,2] = sorts:merge([2,1]),
    [1,2]= sorts:merge([1,2]),
    [1,2] = sorts:quick([2,1]),
    [] = sorts:quick([]).
