-module(multiple_if_example).
-export([check_multiple_conditions/1]).

check_multiple_conditions(Number) ->
    if
        Number > 0, Number rem 2 == 0 ->
            io:fwrite("The number is positive and even.~n");
        Number > 0, Number rem 2 /= 0 ->
            io:fwrite("The number is positive and odd.~n");
        Number < 0 ->
            io:fwrite("The number is negative.~n");
        true ->
            io:fwrite("The number is zero.~n")
    end.
