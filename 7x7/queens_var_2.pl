% Solve 8 Queens - No starting positions
% Example usage: eight_queens(Queens).

% Define the predicate to check if two queens are safe from attacking each other.
safe(Queens) :-
    safe(Queens, 1).

% Base case: An empty list of queens is safe.
safe([], _).
% Recursive case: Check if the first queen is safe from the rest.
safe([Q|Queens], Col) :-
    safe(Queens, Col),
    no_attack(Q, Queens, 1).

% Check if a queen Q is safe from attacking other queens.
no_attack(_, [], _).
no_attack(Q, [Q2|Queens], Dist) :-
    Q =\= Q2,
    abs(Q - Q2) =\= Dist,
    NextDist is Dist + 1,
    no_attack(Q, Queens, NextDist).

% Generate a solution for the Eight Queens problem.
eight_queens(Queens) :-
    permutation([1,2,3,4,5,6,7,8], Queens),
    safe(Queens).