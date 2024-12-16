-module(synchronization_example).
-export([start/0, worker/0, controller/1]).

start() ->
    %% Spawn the worker process
    WorkerPid = spawn(?MODULE, worker, []),
    io:format("Spawned worker with Pid: ~p~n", [WorkerPid]),

    %% Start the controller process
    controller(WorkerPid).

worker() ->
    %% Worker process loop to receive messages
    receive
        {compute, From, N} ->
            Result = factorial(N),
            From ! {result, self(), Result},
            worker();
        {stop, From} -> %% Correctly bind From here
            io:format("Worker stopping...~n"),
            From ! stop_ack,
            ok
    end.

controller(WorkerPid) ->
    %% Controller sends a compute message
    WorkerPid ! {compute, self(), 5},
    
    %% Wait for the result from the worker
    receive
        {result, Worker, Result} ->
            io:format("Received result ~p from ~p~n", [Result, Worker])
    end,
    
    %% Stop the worker
    WorkerPid ! {stop, self()},
    
    %% Confirm the worker stopped
    receive
        stop_ack ->
            io:format("Worker has stopped~n")
    after 5000 ->
        io:format("Timeout waiting for worker to stop~n")
    end.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
