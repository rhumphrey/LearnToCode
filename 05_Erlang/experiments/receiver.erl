-module(receiver).
-export([start/0, loop/0]).

start() ->
    Pid = spawn(fun loop/0),
    io:format("Receiver process started with Pid: ~p~n", [Pid]),
    Pid.

loop() ->
    receive
        {hello, Sender} ->
            io:format("Received hello from Pid: ~p~n", [Sender]),
            Sender ! {hello_ack, self()},
            io:format("Sent hello_ack to Pid: ~p~n", [Sender]),
            loop();
        {hello_ack, Sender} ->
            io:format("Received hello_ack from Pid: ~p~n", [Sender]),
            loop()
    end.