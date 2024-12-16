-module(dispatch_example).
-export([start/0, send_message/2, stop/1]).

%% API functions
start() ->
    spawn(fun() -> loop() end).

send_message(Pid, Message) ->
    Pid ! {message, Message}.

stop(Pid) ->
    Pid ! stop.

%% Process loop
loop() ->
    receive
        {message, Msg} ->
            io:format("Received message: ~p~n", [Msg]),
            loop();
        stop ->
            io:format("Stopping process.~n"),
            ok;
        _Other ->
            io:format("Unknown message received.~n"),
            loop()
    end.

