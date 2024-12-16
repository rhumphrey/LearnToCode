-module(case_example).
-export([check_case/1]).

check_case(Number) ->
    case Number of
        N when N > 0 ->
            io:fwrite("The number is positive.~n");
        N when N < 0 ->
            io:fwrite("The number is negative.~n");
        0 ->
            io:fwrite("The number is zero.~n")
    end.