-module(case_example2).
-export([describe/1, run/0]).

describe(apple) -> fruit;
describe(banana) -> fruit;
describe(carrot) -> vegetable;
describe(_) -> unknown.

run() ->
    io:format("apple is a ~p~n", [describe(apple)]),
    io:format("banana is a ~p~n", [describe(banana)]),
    io:format("carrot is a ~p~n", [describe(carrot)]),
    io:format("stone is a ~p~n", [describe(stone)]).
