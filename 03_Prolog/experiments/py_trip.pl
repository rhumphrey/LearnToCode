% Predicate: is_triple(A, B, C)
% Purpose: Checks if A, B, C form a Pythagorean triple (A^2 + B^2 = C^2)
is_triple(A, B, C) :- D is C*C - A*A - B*B, D = 0.      % Calculate D as the difference and check if it's zero

% Predicate: solve_triple(N, A, B, C)
% Purpose: Generates Pythagorean triples where A, B, and C are between 1 and N
solve_triple(N, A, B, C) :-
    between(1, N, A),                                   % Generate A between 1 and N
    between(1, N, B),                                   % Generate B between 1 and N
    A =< B,                                             % Ensure A is less than or equal to B
    between(1, N, C),                                   % Generate C between 1 and N
    B =< C,                                             % Ensure B is less than or equal to C
    is_triple(A, B, C).                                 % Check if A, B, C form a Pythagorean triple

% Example usage:
% To find Pythagorean triples with elements from 1 to 10, query Prolog with:
% ?- solve_triple(10, A, B, C).