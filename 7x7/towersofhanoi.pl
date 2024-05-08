% From tutorialspoint.com - https://www.tutorialspoint.com/prolog/prolog_towers_of_hanoi_problem.htm

% Tower of Hanoi implementation in Prolog

% Base case: When there is only one disk (N = 1)
move(1, X, Y, _) :-
    write('Move top disk from '), write(X), write(' to '), write(Y), nl.

% Recursive case: When there are more than one disks (N > 1)
move(N, X, Y, Z) :-
    N > 1,
    M is N - 1,
    % Recursively move the top M disks from X to Z (using Y as auxiliary peg)
    move(M, X, Z, Y),
    % Move the largest disk (bottom disk) from X to Y
    move(1, X, Y, _),
    % Recursively move the M disks from Z to Y (using X as auxiliary peg)
    move(M, Z, Y, X).

% Usage query example = move(3, 'A', 'B', 'C').