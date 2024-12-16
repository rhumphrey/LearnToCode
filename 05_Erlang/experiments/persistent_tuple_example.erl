-module(persistent_tuple_example).
-export([run/0]).

run() ->
    Tuple1 = {a, b, c},
    io:format("Original Tuple: ~p~n", [Tuple1]),

    % Changing an element in the tuple
    Tuple2 = setelement(2, Tuple1, x),
    io:format("New Tuple after changing element 2 to x: ~p~n", [Tuple2]).