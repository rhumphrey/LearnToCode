-module(conditional_example).
-export([check_number/1, run/0]).

check_number(Number) ->
    if
        Number > 0 -> positive;
        Number < 0 -> negative;
        Number == 0 -> zero
    end.

run() ->
    io:format("Number 5 is ~p~n", [check_number(5)]),
    io:format("Number -3 is ~p~n", [check_number(-3)]),
    io:format("Number 0 is ~p~n", [check_number(0)]).
