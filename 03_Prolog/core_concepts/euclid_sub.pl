% Facts (none needed for this specific implementation)
% Facts are typically used to state known truths about the world, but in this case, we don't need any.

% Rules
% Rules define relationships and computations in Prolog.

% Rule 1: Base case when Y is 0
euclid(X, 0, X) :- X > 0.                   % If X is positive and Y is 0, the GCD (greatest common divisor) is X.

% Rule 2: Base case when X is 0
euclid(0, Y, Y) :- Y > 0.                   % If X is 0 and Y is positive, the GCD is Y.

% Rule 3: Recursive case
euclid(X, Y, Result) :-
    X > 0,
    Y > 0,
    (X > Y ->
        Temp is X - Y,
        euclid(Temp, Y, Result)
    ;
        Temp is Y - X,
        euclid(X, Temp, Result)
    ).
% If both X and Y are positive, we recursively compute the GCD:
%   - If X > Y, subtract Y from X (Temp = X - Y) and recursively find the GCD of Temp and Y.
%   - Otherwise, subtract X from Y (Temp = Y - X) and recursively find the GCD of X and Temp.

% Query to test
% ?- euclid(36, 63, GCD).
% This query computes the GCD of 36 and 63 and binds the result to the variable GCD.
% The expected output should be 9 (since GCD(36, 63) = 9).