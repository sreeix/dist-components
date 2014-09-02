-module(bloom).
-export([new/2, new/1, add/2, not_exist/2, exist/2, size/1,popcount/1]).

-include_lib("eunit/include/eunit.hrl").

%% Create a tuble with a binary of specific size and Number of hashes to perform.
new(Size, NumHashes) ->
    {<<0:Size>>, NumHashes}.
%% Default number of hashes to 2
new(Size) ->
    new(Size, 2).

%% Adds the Item to the Bloom filter.
%% A new Bloom filter with the applied data is returned. The Alogrithm is as follows
%% Use Crc32 Hash multiple times(restricts the size of the filter to 2 ** 32 ) as specified by NumHashes and bitwise or with the existing index.
add(Item, {HashBinary, NumHashes}) ->
    Size = bit_size(HashBinary),
    <<Hash:Size>> = HashBinary,
    NewHash = lists:foldl(fun(X, Acc) -> (erlang:crc32([Item, X]) rem Size) bor Acc end, Hash, lists:seq(1, NumHashes)),
    {<<NewHash:Size>>, NumHashes}.
%% Recreates the same hash and then tries to find
not_exist(Item, {HashBinary, NumHashes}) ->
    Size = bit_size(HashBinary),
    <<Hash:Size>> = HashBinary,
    ExpectedHash = lists:foldl(fun(X, Acc) -> (erlang:crc32([Item, X]) rem Size) bor Acc end, 0, lists:seq(1, NumHashes)),
    not((Hash band ExpectedHash) == ExpectedHash).

%% This will give false positives. and is not going to be right all the time
exist(Item, BT) ->
    not(not_exist(Item, BT)).


popcount(0) -> 0;
popcount(X)
when is_integer(X), X > 0, X < 16#FFFFFFFF ->
    ((c4(X) bsr 16) + c4(X)) band 16#0000FFFF.
c1(V) -> V - ((V bsr 1) band 16#55555555).
c2(V) -> ((c1(V) bsr 2) band 16#33333333) + (c1(V) band 16#33333333).
c3(V) -> ((c2(V) bsr 4) + c2(V)) band 16#0F0F0F0F.
c4(V) -> ((c3(V) bsr 8) + c3(V)) band 16#00FF00FF.


%% Gives approximate elements in the filter. Based on blah blah
size({HashBinary, NumHashes}) ->
    Size = bit_size(HashBinary),
    <<Hash:Size>> = HashBinary,
    (Size * math:log( 1- (popcount(Hash)/Size))) / NumHashes.

bf_test() ->
    BF = bloom:new(10, 2), %% 10 bits empty bloom filter 1- 1023
    true = bloom:not_exist(1, BF),
    true = bloom:not_exist(2, BF),
    BF1 = bloom:add(10, BF),
    false = bloom:not_exist(10, BF1),
    true = bloom:not_exist(1, BF1).
