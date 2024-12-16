-module(bitwise_example).
-export([run/0]).

run() ->
    A = 4, % Binary: 100
    B = 2, % Binary: 010
    io:format("A band B: ~p~n", [A band B]),
    io:format("A bor B: ~p~n", [A bor B]),
    io:format("A bxor B: ~p~n", [A bxor B]),
    io:format("bnot A: ~p~n", [bnot A]),
    io:format("A bsl 1: ~p~n", [A bsl 1]),
    io:format("A bsr 1: ~p~n", [A bsr 1]).
