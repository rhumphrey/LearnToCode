-module(while).
-export([while/1, while/2, start/0]).

% Entry function with default accumulator
while(List) -> 
    while(List, 0).

% Base case: empty list
while([], Acc) -> 
    Acc;

% Recursive case: process head and recurse on tail
while([_ | Tail], Acc) ->
    io:fwrite("~w~n", [Acc]),
    while(Tail, Acc + 1).

% Start function to initiate the process
start() -> 
    List = [1, 2, 3, 4],
    while(List).
