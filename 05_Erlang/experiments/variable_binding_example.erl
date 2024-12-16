-module(variable_binding_example).
-export([run/0]).

run() ->
    X = 10,
    T = {ok, X},
    io:format("Value: ~p~n", [T]).