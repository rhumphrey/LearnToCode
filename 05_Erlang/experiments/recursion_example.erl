-module(recursion_example).
-export([countdown/1, run/0]).

countdown(0) ->
    io:format("Liftoff!~n");
countdown(N) when N > 0 ->
    io:format("~p...~n", [N]),
    countdown(N - 1).

run() ->
    countdown(5).
