-module(boolean_example).
-export([run/0]).

run() ->
    True = true,
    False = false,
    io:format("True and False: ~p~n", [True and False]),
    io:format("True or False: ~p~n", [True or False]),
    io:format("not True: ~p~n", [not True]),
    io:format("True xor False: ~p~n", [True xor False]).
