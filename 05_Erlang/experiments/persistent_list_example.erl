-module(persistent_list_example).
-export([run/0]).

run() ->
    List1 = [1, 2, 3],
    io:format("Original List: ~p~n", [List1]),

    % Adding an element to the head
    List2 = [0 | List1],
    io:format("New List after adding 0 to the head: ~p~n", [List2]),

    % Removing the head element
    [_Head | Tail] = List2,
    io:format("New List after removing the head: ~p~n", [Tail]).
