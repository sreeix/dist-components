-module(flavius).
-export([last_to_die/2]).
-include_lib("eunit/include/eunit.hrl").

run([Last | [] ], _, _) ->
    io:format("Matching the last remaining persion ~n"),
    Last;

run(People, Skip, KillPosition) ->
    io:format(" running at ~p ~n", [KillPosition]),
    NewKillPosition = case KillPosition > length(People) of true -> KillPosition - length(People); false -> KillPosition end,
    run(lists:delete(lists:nth(NewKillPosition, People), People), Skip, NewKillPosition + Skip - 1).

last_to_die(PeopleCount, Skip) ->
    run(lists:seq(1, PeopleCount, 1), Skip, 1).
