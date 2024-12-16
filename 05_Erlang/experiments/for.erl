-module(for).
-export([for/1, start/0]).

% Base case: when the countdown reaches 0
for(0) ->
    io:fwrite("Liftoff!~n");

% Recursive case: countdown from N to 0
for(N) when N > 0 ->
    io:fwrite("Countdown: ~p~n", [N]),
    for(N-1).

% Start function to initiate the countdown from 5
start() ->
    for(5).