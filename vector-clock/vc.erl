-module(vc).
-export([new/0, increment/1, increment/2, is_concurrent/2, merge/2, prune/1, is_before/2]).
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
increment({LocalProcess, LocalClock}) ->
    {LocalProcess, dict:store(LocalProcess, fetch_with_default(LocalProcess, 0, LocalClock) + 1, LocalClock)}.


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
    dict:merge(fun(_Key, Val1, Val2) -> max(Val1, Val2) end, LocalClock, RemoteClock).

%%%%% This message is sent for updating the process' Clock from a remote process. We refer to remote as the sending process. That clock does not change.
%% The Local process is the process to which the message was sent and that Clock changes

increment({LocalProcess, LocalClock}, {RemoteProcess,RemoteClock}) ->
    increment({LocalProcess, sync_local_clock_from_remote(update_local_counter(RemoteProcess, LocalClock, RemoteClock), RemoteClock)}).

is_concurrent({Process, Clock1}, {Process, Clock2}) ->
    fetch_with_default(Process, 0, Clock1) =:= fetch_with_default(Process, 0, Clock2);

is_concurrent(Event1, Event2) ->
    not(is_before(Event2, Event1)) andalso not(is_before(Event1, Event2)).

merge(Clock1, Clock2) ->
    dict:merge(Clock1, Clock2).

is_before({Process, Clock1}, {Process, Clock2}) ->
    fetch_with_default(Process, 0, Clock1) < fetch_with_default(Process, 0, Clock2);

is_before({Process1, Clock1}, {_, Clock2}) ->
    fetch_with_default(Process1, 0, Clock1) < fetch_with_default(Process1, 0, Clock2).

%% Not implemented
prune(Clock) ->
    Clock.

%% Tests

increment_test() ->
    E1 = {process1, vc:new()},
    E2 = {process2, vc:new()},
    E3 = {process3, vc:new()},
    %% Local Increment
    {_, C1a} = vc:increment(E1),
    1 = dict:fetch(process1, C1a),
    error = dict:find(process2, C1a), %% does not exist should default to 0

     {_, C2l} = vc:increment(E2),
    1 = dict:fetch(process2, C2l),
    error = dict:find(process1, C2l),

    %% Remote Increment.
    {_, C2m} = vc:increment({process2, C2l}, {process1, C1a}),
    2 = dict:fetch(process2, C2m),
    2 = dict:fetch(process1, C2m),
    {_, C1b} = vc:increment({process1, C1a}),
    2 = dict:fetch(process1, C1b),

    {_, C3v} = vc:increment(E3),
    {_, C3w} = vc:increment({process3, C3v}),
    2 = dict:fetch(process3, C3w),
    {_, C2n} = vc:increment({process2, C2m}, {process3, C3w}),

    2 = dict:fetch(process1, C2n),
    3 = dict:fetch(process2, C2n),
    3 = dict:fetch(process3, C2n).


same_process_before_test() ->
    E1 = {process1, vc:new()},

    %% Local Increment
    E1a = vc:increment(E1),
    E1b = vc:increment(E1a),
    E1c = vc:increment(E1b),
    true = vc:is_before(E1a, E1b),
    true = vc:is_before(E1b, E1c),
    false = vc:is_before(E1b, E1a),
    false = vc:is_before(E1b, E1b),
    true = vc:is_concurrent(E1b, E1b),
    false = vc:is_concurrent(E1b, E1c).

different_process_before_test() ->
    E1 = {process1, vc:new()},
    E2 = {process2, vc:new()},
    E3 = {process3, vc:new()},

    %% Local Increment
    {_, C1a} = vc:increment(E1),
    {_, C2l} = vc:increment(E2),
    {_, C3v} = vc:increment(E3),
    {_, C3w} = vc:increment({process3, C3v}),
    {_, C2m} = vc:increment({process2, C2l}, {process1, C1a}),
    {_, C2n} = vc:increment({process2, C2m}, {process3, C3w}),

    {_, C1b} = vc:increment({process1, C1a}),
    {_, C3x} = vc:increment({process3, C3w}),

    {_, C1c} = vc:increment({process1, C1b}),

    {_, C1d} = vc:increment({process1, C1c}, {process3, C3x}),

    {_, C2o} = vc:increment({process2, C2n}),
    {_, C3y} = vc:increment({process3, C3x}),

    {_, C3z} = vc:increment({process3, C3y}, {process2, C2o}),

    E2p = vc:increment({process2, C2o}, {process1, C1c}),
    E2q = vc:increment(E2p),
    true = vc:is_concurrent({process2, C2l}, {process3, C3v}),
    true = vc:is_concurrent({process1, C1d}, {process3, C3z}),

    true = vc:is_concurrent({process2, C2l}, {process1, C1b}),
    false = vc:is_concurrent({process1, C1a}, {process2, C2o}),

    true = vc:is_before({process1, C1b}, E2q),
    true = vc:is_before({process3, C3w}, {process2, C2n}),
    false = vc:is_before(E2q, {process1, C1c}),
    true = vc:is_before({process1, C1a}, {process3, C3z}). %% transitivity. p1 has not communicated to p3 directly
