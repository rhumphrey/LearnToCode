-module(immutability_example).
-export([run/0]).

run() ->
    %% Initial Variable Assignment
    X = 10,
    io:format("Initial value of X: ~p~n", [X]),

    %% Attempt to Reassign Variable
    %% Uncommenting the next line will cause an error
    %% X = 20,

    %% Create a New Variable Instead
    Y = X + 5,
    io:format("Value of Y (X + 5): ~p~n", [Y]),

    %% Demonstrate Immutability in Function Calls
    io:format("Incremented value: ~p~n", [increment(X)]),
    io:format("Original value of X remains unchanged: ~p~n", [X]).

increment(Value) ->
    Value + 1.
