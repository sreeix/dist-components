-module(two_phase_commit).
-export([init/1, execute/2]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").





init(NodePids) ->
    NodePids.

execute(NodePids,Command) ->
    {Coordinator, Slaves} = select_coordinator(NodePids),
    Coordinator ! {self(), commit, Command, Slaves},
    {}.

select_coordinator(Nodes=[]) ->
    erlang:error({noNodesInSystem, Nodes});

select_coordinator([First| Rest]) ->
    {First, Rest}.

loop({TxnRequestor, Command, NodeStatuses}) ->
    receive
        %% Coordinator Stuff
        {From, startCommit, Command, Nodes} ->
            io:format("Coordinator ~p receives command to Commit ~p command on Nodes ~p ~n", [self(), Command, Nodes]),
            loop({From, Command, lists:map(Nodes, fun(N) ->{N, preparing} end)});

        {From, ok, Command} ->
            io:format("Coordinator ~p receives ok to commit from Node ~p ~n", [self(), From]),
            loop({TxnRequestor, Command, update_status(NodeStatuses, {From, ok})});
        {From, abort, Command} ->
            io:format("Coordinator ~p receives abort from  Node ~p ~n", [self(),  From]),
            loop({TxnRequestor, Command, update_status(NodeStatuses, {From, abort})});

        {From, ack, Command} ->
            io:format("Coordinator ~p receives final Ack from  Node ~p ~n", [self(),  From]),
            loop({TxnRequestor, Command, update_status(NodeStatuses, {From, ack})});

        %% Slaves Stuff
        {From, prepare, Command} ->
            io:format("Slave ~p receive command to Prepare ~p  ~n", [self(), Command]),
            loop();
        {From, commit, Command} ->
            io:format("Slave ~p receive command to Commit ~p  ~n", [self(), Command]),
            loop();
        {From, abort, Command} ->
            io:format("Slave ~p receive command to Abort ~p command ~n", [self(), Command]),
            loop()

    end.

update_status(NodeStatuses, NodeStatus) ->
    %% match by key and update status and return whole statues.
    NodeStatuses.

start(NumNodes) ->
    init([ spawn(loop/1, {})|| _ <-lists:seq(1, NumNodes)]).
