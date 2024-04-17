% Base case: The factorial of 0 is 1
factorial(0, 1).

% Recursive case: The factorial of N is N times the factorial of N-1
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.