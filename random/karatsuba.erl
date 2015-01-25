-module(karatsuba).
%% -export([multiply/2]).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

multiply(A,B) when A < 10, B < 10 ->
    A * B;

multiply(A, B) ->
    N = base_for(A, B),
    {X0, X1} = split(A, N),
    {Y0, Y1} = split(B, N),
    Z0 = multiply(X0, Y0),
    Z2 = multiply(X1, Y1),
    Z1 = multiply(X0 + X1, Y0 + Y1) - Z0 - Z2,
   trunc( Z0 * math:pow(10, N * 2) + Z1 * trunc(math:pow(10, N))+ Z2).


base_for(A, B) ->
    (trunc(math:log10(max(A, B))) + 1) div 2.


split(A, N) ->
    Num =trunc(math:pow(10, N)),
    {A div Num, A rem Num}.


multiply_test() ->
    0 = multiply(0, 0),
    1 = multiply(1, 1),
    24 = multiply(2, 12),
    1000 = multiply(10 , 100),
    55 = multiply(5, 11).
