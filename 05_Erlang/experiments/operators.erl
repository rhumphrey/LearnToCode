-module(operators). 
-export([start/0]).

start() -> 
    X = 3,
    Y = 2,
    A = true,
    B = false,
    io:fwrite("~w~n",[X + Y]),
    io:fwrite("~w~n",[X - Y]),
    io:fwrite("~w~n",[X * Y]),
    io:fwrite("~w~n",[X / Y]),
    io:fwrite("~w~n",[X rem Y]),
    io:fwrite("~w~n",[X div Y]),
    io:fwrite("~n"),
    io:fwrite("~w~n",[X == Y]),
    io:fwrite("~w~n",[X /= Y]),
    io:fwrite("~w~n",[X < Y]),
    io:fwrite("~w~n",[X =< Y]),
    io:fwrite("~w~n",[X > Y]),
    io:fwrite("~w~n",[X >= Y]),
    io:fwrite("~n"),
    io:fwrite("~w~n",[A or B]),
    io:fwrite("~w~n",[A and B]),
    io:fwrite("~w~n",[not A]),
    io:fwrite("~w~n",[A xor B]).
