-module(cached).
-export([put/2, get/1]).
-export([start/0, loop/1]).



rpc(Message, Timeout) ->
    cached ! {self(), Message},
    receive
	{ok, Response} -> io:format("Got Response ~p~n", [Response]);
	{error, Why} -> io:format("Failed because of ~p~n", [Why])
    after Timeout -> io:format("Timing out after ~p ms", [Timeout])
    end.
rpc(Message) ->
    rpc(Message, 5000).

put(Key, Value) ->
    rpc({put, Key, Value}).

get(Key) ->
    rpc({get, Key}).


loop(T) ->
    receive
	{From, _X = {put, Key, Value}} ->
	    ets:insert(T, {Key, Value}),
	    From ! {ok, true},
	    loop(T);
	{From, {get, Key}} ->
	    io:format("Got ~p to get ~n", [Key]),
	    Value = case ets:lookup(T, Key) of
			[{_, V}] -> V;
			[] -> not_found
		    end,
	    From ! {ok, Value},
	    loop(T);
	{From, stop} ->
	    io:format("Exiting becasse of stop signal from ~p ~n", [From])
    end.

init() ->
    T = ets:new(cachedTable, [set, public]),
    Pid = spawn_link(cached, loop, [T]),
    ets:give_away(cachedTable, Pid, gift),
    receive
	{'EXIT', Pid, normal} ->
	    io:format("Normal shutdown of ~p ~n", [Pid]);
	{'EXIT', Pid, shutdown} ->
	    io:format("Shutdown of the process ~p ~n", [Pid]);
	{'EXIT', Pid, _} -> init() %% Should receive the table from the owner and pass it to the new PID
    end.

start() ->
    spawn(cached, init, []).
