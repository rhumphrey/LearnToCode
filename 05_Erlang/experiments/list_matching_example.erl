-module(list_matching_example).
-export([run/0]).

run() ->
    [Head | Tail] = [1, 2, 3, 4],
    io:format("Head: ~p, Tail: ~p~n", [Head, Tail]).
