-module(sender).
-export([start/1]).

start(Pid) ->
    Pid ! {hello, self()},
    io:format("Message sent to Pid: ~p~n", [Pid]),
    receive
        {hello_ack, Sender} ->
            io:format("Received hello_ack from Pid: ~p~n", [Sender])
    after 5000 ->
        io:format("Timeout waiting for hello_ack~n")
    end.