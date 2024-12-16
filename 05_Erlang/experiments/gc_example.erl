-module(gc_example).
-export([start/0, worker/1]).

start() ->
    %% Spawn multiple worker processes
    Pid1 = spawn(?MODULE, worker, [1000000]),
    Pid2 = spawn(?MODULE, worker, [2000000]),
    Pid3 = spawn(?MODULE, worker, [3000000]),
    io:format("Spawned worker processes with Pids: ~p, ~p, ~p~n", [Pid1, Pid2, Pid3]).

worker(N) ->
    %% Perform some computation
    Result = lists:seq(1, N),
    io:format("Process ~p calculated result with length: ~p~n", [self(), length(Result)]),

    %% Simulate some work and wait for a message
    receive
        stop ->
            io:format("Process ~p stopping...~n", [self()])
    end.