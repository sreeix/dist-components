-module(consistent).
-export([new/0, new/1, add_node/2, remove_node/2,  node_for/2]).

- define (TOPOFCLOCK, 359).
-include_lib("eunit/include/eunit.hrl").

new() ->
    new(1).

new(NumRepeats) ->
    {NumRepeats, []}.

%% Internal
to_string(V) ->
    if
	is_atom(V) -> atom_to_list(V);
	is_integer(V) -> integer_to_list(V);
	true -> V
    end.

sha_to_number(ToSha) ->
   crypto:bytes_to_integer(crypto:hash(sha, ToSha)) rem ?TOPOFCLOCK.

add_node({Id, _}, {NumRepeats, Partitions}) ->
    Val = to_string(Id),
    NewPartitions = [ {Id, sha_to_number(Val ++ integer_to_list(X))} ||  X <- lists:seq(1, NumRepeats, 1)],
    {NumRepeats, lists:keysort(2,  Partitions ++ NewPartitions)}.

remove_node({Id, _}, {NumRepeats, Partitions}) ->
    [NumRepeats, lists:dropwhile(fun({X,_}) -> X =:= Id end, Partitions)].


node_for(Key, {_, Partitions}) ->
    RingLocation = sha_to_number(Key),
    %% Use comprehensions ....
    case lists:dropwhile(fun({_, NodeLocation})  -> NodeLocation < RingLocation end, Partitions) of
	[] -> if length(Partitions) =:= 0 ->{no_node_error, length(Partitions)};
		 true -> {ok, hd(Partitions)}
	      end;
	Match -> {ok, hd(Match)}
    end.

%% Tests

add_node_test() ->
    CHashNoNode = consistent:new(),
    CHashWithOneNode = consistent:add_node({100,{ram, 1000}}, CHashNoNode ),
    {no_node_error, 0} = consistent:node_for("Test String", CHashNoNode),
    {ok, {100, _}}= consistent:node_for("Test String", CHashWithOneNode),
    CHashWithTwoNodes = consistent:add_node({100,{ram, 1000}}, CHashWithOneNode ),
    {ok, {100, _}}= consistent:node_for("Test String", CHashWithTwoNodes).
