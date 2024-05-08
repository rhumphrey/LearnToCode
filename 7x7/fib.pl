fib(0, 1) :-
    !.
fib(N, F) :-
    fib(1, N, 1, 1, F).

fib(N, N, _, F, F) :-
    !.
fib(N0, N, F0, F1, F) :-
    N1 is N0 + 1,
    F2 is F0 + F1,
    fib(N1, N, F1, F2, F).