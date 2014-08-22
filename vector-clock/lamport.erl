-module(lamport).
-export([new/1, increment/1, set_counter/3, is_concurrent/2, is_before/2]).
-include_lib("eunit/include/eunit.hrl").
%% Lamports clock impementation, with the small quick(lamport clocks only define total ordering, and not answer questions like is before)
new(Process) ->
    [{Process, 0}].

%% Increments the counter by one or sets creates one for the process.
%% This is used by the current process internally.
increment(Clock) -> [{element(1, hd(Clock)), element(2, hd(Clock)) + 1} | Clock].

%% When another process makes a call to the current process, it needs to set its counter to
%% something higher than the sending process count. This means that if local count his higher
%% then it increments that, otherwise sets it to the next value. So it's Process counter is
%% higher than the sending process Count

set_counter(Process, Count, Clock) ->
    [{P1, LatestClockCount} | _Tail] = Clock,
    NewClock = [{Process, Count} | Clock],
    [{P1, case Count > LatestClockCount of
	true -> Count;
	false -> Clock
    end + 1} | NewClock].

%% This function is non reflexive.
is_concurrent([{Process1, Count1} | _Tail1], Clock2) ->
    [{Process2, Count2} | _Tail2] = Clock2,
    (Process1 =:= Process2 andalso Count1 =:= Count2)  or is_concurrent_diff_process(Process1, Count1, Clock2).

%% We use the simple process number to do the total ordering. This means that the process that is more actuve will generally win. The other alternative would be as Lamport suggests to use process priority. implementation commented out after this function. This does not exactly follow the lamport algorithm, which just keeps counts and not the process that sent the message. This way we can implement concurrent/before a wee bit better. This is generalized in the fidge's vector clock

is_before([{_, Count1}| _], [{_, Count2} | _]) ->
    Count1 < Count2.


%% Left here as a remenent of ho papers have many possible implementations.
%% is_before(Clock1, Clock2) ->
%%     [{P1, C1} | _Tail] = Clock1,
%%     [{P2, C2} | _Tail] = Clock2,
%%    (C1< C2 andalso P1 =:= P2) or (is_concurrent_diff_process(P1, C1, Clock2) andalso P1 <P2).


 %% I could not find any more element from this process, This means that Process1 has never communicated to the other process hence we cannot positively commit if it is before/after the other clock, hence concurrent.
is_concurrent_diff_process(_Process1, _Count1, []) ->
    true;

is_concurrent_diff_process(Process1, Count1, [{Process2, Count2} | Tail2]) ->
    case [Process1 =:= Process2, Count1 =< Count2] of
	[true, true] -> true;
	[true, false] ->
	    false;
	_NotSameProcessName -> is_concurrent_diff_process(Process1, Count1, Tail2)
end.


%% Tests

same_process_ordering_test() ->
    C = lamport:new(process1),
    C1 = lamport:increment(C),
    true = lamport:is_before(C, C1),
    false = lamport:is_before(C1, C),
    true = lamport:is_concurrent(C1, C1),
    false = lamport:is_concurrent(C1, C).

different_process_ordering_test() ->
    CP1 = lamport:new(process1),
    CP2 = lamport:new(process2),
    C1P1 = lamport:increment(CP1),
    false = lamport:is_before(CP1, CP2),
    true = lamport:is_concurrent(CP1, CP2),
    true = lamport:is_concurrent(C1P1, CP2),
    %% Send message from C1P1 to CP2. So evertthing after CP2 should be after C1P1
    C2P2 = lamport:set_counter(process1, element(2, hd(C1P1)), CP2),
    true = lamport:is_before(C1P1, C2P2),
    true = lamport:is_before(CP1, C2P2).
