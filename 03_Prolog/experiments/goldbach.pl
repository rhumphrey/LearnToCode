% Define a predicate to check if a number is prime.
is_prime(2).                                    % 2 is prime
is_prime(3).                                    % 3 is prime
is_prime(Number) :- 
    integer(Number),                            % Check if Number is an integer
    Number > 3,                                 % Ensure Number is greater than 3
    Number mod 2 =\= 0,                         % Number should not be even
    \+ has_factor(Number, 3).                   % Number should not have any factor starting from 3

% Define a predicate to check if a number has a factor.
has_factor(Number, Factor) :- 
    Number mod Factor =:= 0.                    % Number is divisible by Factor
has_factor(Number, Factor) :- 
    Factor * Factor < Number,                   % Square of Factor is less than Number
    NextFactor is Factor + 2,                   % Increment Factor by 2 (check only odd numbers)
    has_factor(Number, NextFactor).             % Recursively check for factors

% Define the main predicate for Goldbach's Conjecture.
goldbach(4, [2, 2]) :- !.                       % Base case for Goldbach's Conjecture when Number is 4
goldbach(Number, PrimesList) :- 
    Number mod 2 =:= 0,                         % Check if Number is even
    Number > 4,                                 % Number should be greater than 4
    goldbach(Number, PrimesList, 3).            % Start checking for prime pairs from 3

goldbach(Number, [Prime1, Prime2], Prime1) :- 
    Prime2 is Number - Prime1,                  % Calculate the second prime
    is_prime(Prime2),                           % Check if the second number is prime
    Prime1 < Prime2,                            % Ensure the first prime is less than the second prime
    !.                                          % Cut to prevent backtracking

goldbach(Number, PrimesList, CurrentPrime) :- 
    CurrentPrime < Number,                      % Ensure the current prime is less than the Number
    next_prime(CurrentPrime, NextPrime),        % Find the next prime number
    goldbach(Number, PrimesList, NextPrime).    % Recursively check for the next prime pair

% Define a predicate to find the next prime number.
next_prime(CurrentPrime, NextPrime) :- 
    NextPrime is CurrentPrime + 2,              % Increment CurrentPrime by 2 to get the next odd number
    is_prime(NextPrime),                        % Check if the next odd number is prime
    !.                                          % Cut to prevent backtracking
next_prime(CurrentPrime, NextPrime) :- 
    NextOdd is CurrentPrime + 2,                % Increment CurrentPrime by 2 to get the next odd number
    next_prime(NextOdd, NextPrime).             % Recursively find the next prime number

% Query example:
% ?- goldbach(26, PrimesList).
% Output: L = [3, 23].