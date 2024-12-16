-module(type_inference_example).
-export([add/2, concatenate/2, compute/1, run/0]).

add(A, B) ->
    A + B.

concatenate(List1, List2) ->
    List1 ++ List2.

compute(Value) ->
    case Value of
        {ok, Number} when is_integer(Number) -> Number * 2;
        {error, Reason} -> Reason;
        _ -> unknown
    end.

run() ->
    io:format("Add: 5 + 3 = ~p~n", [add(5, 3)]),
    io:format("Concatenate: [1, 2] ++ [3, 4] = ~p~n", [concatenate([1, 2], [3, 4])]),
    io:format("Compute: {ok, 10} = ~p~n", [compute({ok, 10})]),
    io:format("Compute: {error, something} = ~p~n", [compute({error, something})]),
    io:format("Compute: 42 = ~p~n", [compute(42)]).