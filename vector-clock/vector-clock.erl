-module('vector-clock').
-export([new/0,increment/2, is_concurrent/3]).

new() ->
    dict:new().

increment(Process, Clock) ->
    case dict:is_key(Process, Clock) of
	true ->
	    dict:update_counter(Process, 1, Clock);
	false  ->
	    dict:store(Process, 0, Clock)
    end.

is_concurrent()
