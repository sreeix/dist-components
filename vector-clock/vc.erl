-module(vc).
-export([new/0,increment/2, is_concurrent/3, merge/2, prune/1, is_before/3]).
%% A clock is associated with each event. This clock is used to figure out weather the event is before or after the event.
%% For some events it is not possible to figure out if they are after/before, and in such cases they are concurrent.
%% new -> creates a new clock.
%% increment -> Increments a clock for the specified process Id. If we see it for the first time it is set to 0(and then incrementd).
%% prune -> remove state on vector clocks.

-include_lib("eunit/include/eunit.hrl").

new() ->
    dict:new().

increment(Process, Clock) ->
    case dict:is_key(Process, Clock) of
	true ->
	    dict:update_counter(Process, 1, Clock);
	false  ->
	    dict:store(Process, 0, Clock)
    end.

%% is_concurrent(Process1, Process)

merge(Clock1, Clock2) ->
    dict:merge(Clock1, Clock2).

%%  Tests

increment_test() ->
    C = vc:new(),
    C1 = vc:increment(10, C),
