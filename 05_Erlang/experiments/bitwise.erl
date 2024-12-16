-module(bitwise).
-export([operations/0]).

operations() ->
    A = 5,  % 0101 in binary
    B = 3,  % 0011 in binary

    %% Display original values in decimal and binary
    io:format("A = ~p (~s)~n", [A, io_lib:format("~4.2B", [A])]),
    io:format("B = ~p (~s)~n", [B, io_lib:format("~4.2B", [B])]),

    %% Bitwise AND
    AndResult = A band B,
    io:format("~p band ~p = ~p (~s)~n", [A, B, AndResult, io_lib:format("~4.2B", [AndResult])]),

    %% Bitwise OR
    OrResult = A bor B,
    io:format("~p bor ~p  = ~p (~s)~n", [A, B, OrResult, io_lib:format("~4.2B", [OrResult])]),

    %% Bitwise XOR
    XorResult = A bxor B,
    io:format("~p bxor ~p = ~p (~s)~n", [A, B, XorResult, io_lib:format("~4.2B", [XorResult])]),

    %% Bitwise NOT
    NotResult = bnot A,
    io:format("bnot ~p  = ~p (~s)~n", [A, NotResult, io_lib:format("~.2B", [NotResult band 255])]),

    %% Bitwise Shift Left
    ShiftLeftResult = A bsl 1,
    io:format("~p bsl 1 = ~p (~s)~n", [A, ShiftLeftResult, io_lib:format("~4.2B", [ShiftLeftResult])]),

    %% Bitwise Shift Right
    ShiftRightResult = A bsr 1,
    io:format("~p bsr 1 =  ~p (~s)~n", [A, ShiftRightResult, io_lib:format("~4.2B", [ShiftRightResult])]).

