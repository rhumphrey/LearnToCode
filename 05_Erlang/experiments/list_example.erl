-module(list_example).
-export([run/0]).

run() ->
    List1 = [1, 2, 3],
    List2 = [4, 5],
    io:format("List1 ++ List2: ~p~n", [List1 ++ List2]),
    io:format("List1 -- [2]: ~p~n", [List1 -- [2]]).
