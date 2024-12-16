-module(messaging_example).
-export([start/0, process/0, send_message/1]).

start() ->
    %% Spawn a new process running the process/0 function
    Pid = spawn(?MODULE, process, []),
    io:format("Spawned process with Pid: ~p~n", [Pid]),

    %% Send a message to the process
    send_message(Pid).

process() ->
    %% Process loop to receive messages
    receive
        {hello, FromHello} ->
            io:format("Received 'hello' from ~p~n", [FromHello]),
            FromHello ! {hello_ack, self()},
            process();
        {stop, FromStop} ->
            io:format("Received 'stop' from ~p, stopping...~n", [FromStop]),
            FromStop ! {stopped, self()},
            ok
    end.

send_message(Pid) ->
    %% Send 'hello' message to the process
    Pid ! {hello, self()},
    receive
        {hello_ack, FromHelloAck} ->
            io:format("Received 'hello_ack' from ~p~n", [FromHelloAck])
    end,

    %% Send 'stop' message to the process
    Pid ! {stop, self()},
    receive
        {stopped, FromStopAck} ->
            io:format("Process ~p stopped~n", [FromStopAck])
    end.