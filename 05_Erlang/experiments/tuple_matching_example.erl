-module(tuple_matching_example).
-export([run/0]).

run() ->
    {ok, Value} = {ok, 42},
    io:format("Value: ~p~n", [Value]).
