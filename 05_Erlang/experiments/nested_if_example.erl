-module(nested_if_example).
-export([check_nested_conditions/1]).

check_nested_conditions(Number) ->
    if
        Number > 0 ->
            if
                Number rem 2 == 0 ->
                    io:fwrite("The number is positive and even.~n");
                true ->
                    io:fwrite("The number is positive and odd.~n")
            end;
        Number < 0 ->
            io:fwrite("The number is negative.~n");
        true ->
            io:fwrite("The number is zero.~n")
    end.