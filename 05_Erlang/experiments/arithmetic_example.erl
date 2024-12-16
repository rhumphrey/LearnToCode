-module(arithmetic_example).
-export([run/0]).

run() ->
    A = 10,
    B = 3,
    io:format("Addition: ~p + ~p = ~p~n", [A, B, A + B]),
    io:format("Subtraction: ~p - ~p = ~p~n", [A, B, A - B]),
    io:format("Multiplication: ~p * ~p = ~p~n", [A, B, A * B]),
    io:format("Division: ~p / ~p = ~p~n", [A, B, A / B]),
    io:format("Integer Division: ~p div ~p = ~p~n", [A, B, A div B]),
    io:format("Remainder: ~p rem ~p = ~p~n", [A, B, A rem B]).
