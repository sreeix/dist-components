-module(ring).
-export([start_central/3, ring_node_loop/1, start_distributed/3, ring_node_loop2/2]).

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


ring_node_loop2(NextProcess, NumMessages) ->
    Forward = fun(Message) ->
		      io:format("Got Message(~p) ~p , Forwarding to ~p ~n", [self(), Message, NextProcess]),
		      case NextProcess of
			  nil -> true;
			  _ -> NextProcess ! Message
		      end
	      end,
    receive
	{start, 0} -> ring_node_loop2(nil, NumMessages); %% Do nothing
	{start, N} -> Pid = spawn(ring, ring_node_loop2, [nil, NumMessages]),
		      Pid ! {start, N -1},
		      %% io:format("New process: ~p~n", [Pid]),
		      ring_node_loop2(Pid, NumMessages);

	{message, {Message, Count}} -> Forward({message, {Message, Count}}),
				       ring_node_loop2(NextProcess, NumMessages);
	{stop} -> Forward({stop})
    end.

%% This is obviously a hack. And not fully distributed, but that needs FSM.
start_distributed(NumMessages, N, Message) ->
    Root = spawn(ring, ring_node_loop2, [nil, NumMessages]),
    Root ! {start, N},
    Root ! {message, {Message, 1}},
    Root ! {stop}.
