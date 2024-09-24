-module(count_to_ten).
-export([count/0]).

count() ->
    count(1).

count(11) -> 
    io:format("Done!~n");
count(N) when N =< 10 ->
    io:format("~p~n", [N]),
    count(N + 1).
