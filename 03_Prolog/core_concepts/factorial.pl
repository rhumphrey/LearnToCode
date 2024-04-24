% Base case: The factorial of 0 is 1.
factorial(0, 1).

% Recursive case: The factorial of N is N times the factorial of N-1.
factorial(N, Result) :-
    N > 0,                                      % When N is greater than 0, we calculate the factorial of N by multiplying N with the factorial of N-1.
    NewN is N - 1,                              % computes the value of N-1.
    factorial(NewN, NewResult),                 % recursive call factorial(N1\ewN, NewResult) calculates the factorial of N-1.
    Result is N * NewResult.                    % Result is computed as N * NewResult.  

% The recursive definition allows us to break down the problem into smaller subproblems (factorials of smaller numbers) 
% until we reach the base case (factorial of 0), at which point we can compute the final result. 

% Query - ?- factorial(5, Result).