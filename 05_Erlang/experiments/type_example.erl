-module(type_example).
-export([add/2, subtract/2, multiply/2, divide/2, run/0]).

-spec add(integer(), integer()) -> integer().
add(A, B) ->
    A + B.

-spec subtract(integer(), integer()) -> integer().
subtract(A, B) ->
    A - B.

-spec multiply(integer(), integer()) -> integer().
multiply(A, B) ->
    A * B.

-spec divide(integer(), integer()) -> {ok, float()} | {error, atom()}.
divide(_A, 0) ->
    {error, division_by_zero};
divide(A, B) ->
    {ok, A / B}.

run() ->
    io:format("Add: 5 + 3 = ~p~n", [add(5, 3)]),
    io:format("Subtract: 5 - 3 = ~p~n", [subtract(5, 3)]),
    io:format("Multiply: 5 * 3 = ~p~n", [multiply(5, 3)]),
    io:format("Divide: 5 / 0 = ~p~n", [divide(5, 0)]),
    io:format("Divide: 5 / 2 = ~p~n", [divide(5, 2)]).
