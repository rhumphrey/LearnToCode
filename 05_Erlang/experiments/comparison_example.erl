-module(comparison_example).
-export([run/0]).

run() ->
    A = 10,
    B = 5,
    io:format("A == B: ~p~n", [A == B]),
    io:format("A /= B: ~p~n", [A /= B]),
    io:format("A < B: ~p~n", [A < B]),
    io:format("A =< B: ~p~n", [A =< B]),
    io:format("A > B: ~p~n", [A > B]),
    io:format("A >= B: ~p~n", [A >= B]).
