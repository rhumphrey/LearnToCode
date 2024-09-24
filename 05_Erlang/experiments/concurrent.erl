-module(concurrent).
-export([start/1, ping/2, pong/0]).

start(N) ->
    % Spawn the pong process
    Pid = spawn(fun pong/0),
    io:format("Pong process spawned with Pid: ~p~n", [Pid]),
    % Send a ping message to the pong process
    Pid ! {ping, self()},
    io:format("Sent initial ping to Pid: ~p~n", [Pid]),
    % Start the ping process with N pings
    ping(N, Pid),
    % Wait for the final pong message
    receive
        pong -> io:format("Pong received in start/1~n")
    end.

ping(0, _) -> io:format("Ping complete~n");
ping(N, Pid) ->
    io:format("Ping ~p~n", [N]),
    receive
        pong ->
            io:format("Received pong~n"),
            Pid ! {ping, self()},
            io:format("Sent ping to Pid: ~p~n", [Pid]),
            ping(N - 1, Pid)
    after 5000 ->
        io:format("Timeout waiting for pong~n")
    end.

pong() ->
    io:format("Pong waiting for message~n"),
    receive
        {ping, Pid} ->
            io:format("Pong received ping from Pid: ~p~n", [Pid]),
            Pid ! pong,
            io:format("Pong sent pong to Pid: ~p~n", [Pid]),
            pong()
    end.

% To run this example:
% 1. Compile the module: c(concurrent).
% 2. Start the process with a specific number of pings: concurrent:start(5).