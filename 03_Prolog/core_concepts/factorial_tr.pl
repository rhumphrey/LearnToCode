% Tail-recursive factorial helper predicate with an accumulator.
% This predicate is used to compute the factorial of N using an accumulator Acc.
factorial_tr(N, Acc, Result) :-
    N > 0,                                                  % Check if N is greater than 0 to continue recursion.
    NewAcc is N * Acc,                                      % Update the accumulator by multiplying it with N.
    NewN is N - 1,                                          % Decrease N by 1 to move towards the base case.
    factorial_tr(NewN, NewAcc, Result).                     % Recurse with the new values.
factorial_tr(0, Acc, Acc).                                  % Base case: when N is 0, the result is the accumulated value.

% User-friendly wrapper predicate for calculating factorial.
% This predicate is what users call to calculate the factorial of N.
factorial(N, Result) :-
    factorial_tr(N, 1, Result).                             % Call the tail-recursive helper with an initial accumulator of 1.


% In this version, factorial_tr/3 is the tail-recursive predicate that does the actual computation, using an accumulator to keep track of the intermediate results. 
% The factorial/2 predicate is a wrapper that you can use to initiate the calculation with the accumulator starting at 1. 
% This version is more efficient because it doesn’t need to keep track of each recursive call; it simply updates the accumulator and proceeds to the next step.

% ?- factorial(5, Result).