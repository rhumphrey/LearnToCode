-module(function_clauses_example).
-export([factorial/1, run/0]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

run() ->
    io:format("Factorial of 5 is ~p~n", [factorial(5)]),
    io:format("Factorial of 0 is ~p~n", [factorial(0)]).
