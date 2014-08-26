-module(ring).
-export([start_central/3, ring_node_loop/1]).

ring_node_loop(NextProcess) ->
    Forward = fun(Message) -> NextProcess ! Message  end,
    receive
	{stop} -> Forward({stop});
	{message, {Text, Count}} -> io:format("Got Message from the process(~p) ~p:  ~p, ~n", [self(), Text, Count]),
				    Forward({message, {Text, Count}}),
				    ring_node_loop(NextProcess)
    end.

start_processes(1) ->
    spawn(ring, ring_node_loop, [self()]); %% send it back to the parent

start_processes(Count) ->
   spawn(ring, ring_node_loop, [start_processes(Count - 1)]).


send_message(Process, Message, Count) ->
    Process ! {message, {Message, Count}},
    receive
	M -> io:format("Got message back from the process ~p ~n", [M])
    end.

start_central(M, N, Message) ->
    Root = start_processes(N),
    [send_message(Root, Message, Count) || Count <- lists:seq(1, M)],
    Root ! {stop}.
