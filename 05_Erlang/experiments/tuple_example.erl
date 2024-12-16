-module(tuple_example).
-export([run/0]).

run() ->
    % Define a tuple
    MyTuple = {apple, banana, cherry},
    
    % Retrieve the second element
    SecondElement = element(2, MyTuple),
    io:format("The second element is: ~p~n", [SecondElement]),
    
    % Update the third element
    UpdatedTuple = setelement(3, MyTuple, grape),
    io:format("The updated tuple is: ~p~n", [UpdatedTuple]),
    
    % Retrieve the updated third element
    ThirdElement = element(3, UpdatedTuple),
    io:format("The updated third element is: ~p~n", [ThirdElement]).
