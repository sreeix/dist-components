-module(vc).
-export([new/0, increment/2, increment/4, is_concurrent/2, merge/2, prune/1, is_before/2]).
%% A clock is associated with each event. This clock is used to figure out weather the event is before or after the event.
%% For some events it is not possible to figure out if they are after/before, and in such cases they are concurrent.
%% new -> creates a new clock.
%% increment -> Increments a clock for the specified process Id. If we see it for the first time it is set to 0(and then incrementd).
%% prune -> remove state on vector clocks.

%% Implementations Notes:
%% Stolen mostly from fidge's paper.
%%  Clock is a dict of processid and the count. And incrementing the
-include_lib("eunit/include/eunit.hrl").

new() ->
    dict:new().

%% This is the local version of increment. simply increments the current process value.
increment(LocalProcess, LocalClock) ->
    dict:store(LocalProcess, fetch_with_default(LocalProcess, 0, LocalClock) + 1, LocalClock).


fetch_with_default(Key, Default, Dict) ->
    case dict:is_key(Key, Dict) of
	false -> Default;
	true  -> dict:fetch(Key, Dict)
    end.

update_local_counter(RemoteProcess, LocalClock, RemoteClock) ->
    LocalClockCount = fetch_with_default(RemoteProcess, 0, LocalClock),
    RemoteClockCount = fetch_with_default(RemoteProcess, 0,  RemoteClock),
    if
	LocalClockCount =< RemoteClockCount -> dict:store(RemoteProcess, 1 + RemoteClockCount, LocalClock);
	true -> LocalClock
    end.

sync_local_clock_from_remote(LocalClock, RemoteClock) ->
    dict:map(fun(Key, Value) -> max(Value, fetch_with_default(Key, 0, RemoteClock)) end, LocalClock).

%%%%% This message is sent for updating the process' Clock from a remote process. We refer to remote as the sending process. That clock does not change.
%% The Local process is the process to which the message was sent and that Clock changes

increment(LocalProcess, RemoteProcess, LocalClock, RemoteClock) ->
    increment(LocalProcess, sync_local_clock_from_remote(update_local_counter(RemoteProcess, LocalClock, RemoteClock), RemoteClock)).

is_concurrent(Clock1, Clock2) ->
    true.

merge(Clock1, Clock2) ->
    dict:merge(Clock1, Clock2).

is_before(Clock1, Clock2) ->
    false.

%% Not implemented
prune(Clock) ->
    Clock.

increment_test() ->
    C1 = vc:new(),
    C2 = vc:new(),
    C3 = vc:new(),
    %% Local Increment
    C1a = vc:increment(process1, C1),
    1 = dict:fetch(process1, C1a),
    error = dict:find(process2, C1a), %% does not exist should default to 0

    C2l = vc:increment(process2, C2),
    1 = dict:fetch(process2, C2l),
    error = dict:find(process1, C2l),

    %% Remote Increment.
    C2m = vc:increment(process2, process1, C2l, C1a),
    2 = dict:fetch(process2, C2m),
    2 = dict:fetch(process1, C2m),
    C1b = vc:increment(process1, C1a),
    2 = dict:fetch(process1, C1b),

    C3v = vc:increment(process3, C3),
    C3w = vc:increment(process3, C3v),
    2 = dict:fetch(process3, C3w),
    C2n = vc:increment(process2, process3, C2m, C3w),
    io:format("Valss : ~p ~p ~n", [dict:fetch(process1, C2n), dict:fetch(process2, C2n)]),

    2 = dict:fetch(process1, C2n),
    3 = dict:fetch(process2, C2n),
    3 = dict:fetch(process3, C2n).
