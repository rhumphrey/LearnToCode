% Base case: If the second number is 0, the GCD is the first number.
euclid_mod(X, 0, X).

% Recursive case: If the second number is not 0, compute the GCD.
euclid_mod(X, Y, Result) :-
    % Calculate the remainder of X divided by Y.
    Remainder is X mod Y,
    % Recursively call euclid_mod with Y and the Remainder to find the GCD.
    euclid_mod(Y, Remainder, Result).