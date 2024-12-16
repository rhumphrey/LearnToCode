-module(car).
-export([new/0, start/1, stop/1, get_status/1]).

% Initialize a new Car process
new() ->
    spawn(fun() -> car_loop(stopped) end).

% Handle car state
car_loop(Status) ->
    receive
        {start, From} ->
            From ! {ok, running},
            car_loop(running);
        {stop, From} ->
            From ! {ok, stopped},
            car_loop(stopped);
        {get_status, From} ->
            From ! {status, Status},
            car_loop(Status)
    end.

start(CarPid) ->
    CarPid ! {start, self()},
    receive
        {ok, Status} -> {ok, Status}
    end.

stop(CarPid) ->
    CarPid ! {stop, self()},
    receive
        {ok, Status} -> {ok, Status}
    end.

get_status(CarPid) ->
    CarPid ! {get_status, self()},
    receive
        {status, Status} -> {status, Status}
    end.