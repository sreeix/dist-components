-module(ring).
-export([start_central/3, ring_node_loop/1]).

ring_node_loop(NextProcess) ->
    Forward = fun(Message) ->
		      case NextProcess of
			  nil -> true;
			  _ -> NextProcess ! Message
		      end
	      end,
    receive
	{stop} -> Forward({stop});
	{message, {Text, Count}} -> io:format("Got Message from the main(~p) ~p:  ~p, ~n", [self(), Text, Count]),
				    Forward({message, {Text, Count}}),
				    ring_node_loop(NextProcess)
    end.

start_processes(1) ->
    spawn(ring, ring_node_loop, [nil]);

start_processes(Count) ->
   spawn(ring, ring_node_loop, [start_processes(Count - 1)]) .

start_central(M, N, Message) ->
    Root = start_processes(N),
    [Root ! {message, {Message, Count}} || Count <- lists:seq(1, M)],
    Root ! {stop}.
