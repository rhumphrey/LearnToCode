-module(if_example).
-export([check_number/1]).

check_number(Number) ->
    if
        Number > 0 ->
            io:fwrite("The number is positive.~n");
        Number < 0 ->
            io:fwrite("The number is negative.~n");
        true ->
            io:fwrite("The number is zero.~n")
    end.