-module(case_expression_example).
-export([classify/1, run/0]).

classify(Number) ->
    case Number of
        N when N > 10 -> large;
        N when N >= 1 -> medium;
        _ -> small
    end.

run() ->
    io:format("Number 15 is ~p~n", [classify(15)]),
    io:format("Number 7 is ~p~n", [classify(7)]),
    io:format("Number -2 is ~p~n", [classify(-2)]).
