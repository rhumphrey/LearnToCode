% Base case: when the countdown reaches 0, cut (!) to stop recursion.
countdown(0) :- !.

% Recursive case: print the current number, decrement it, and call countdown again.
countdown(N) :-
    writeln(N),                                     % Print the current number N.
    N1 is N - 1,                                    % Calculate N1 as N decremented by 1.
    countdown(N1).                                  % Recursively call countdown with the decremented number.

% This predicate initiates a nested countdown based on user input.
nested_countdown :-
    repeat,                                         % Start an infinite loop.
    write('Enter a positive number (0 to stop): '), % Prompt the user for a number.
    read(N),                                        % Read the user's input number.
    ( N =:= 0                                       % If the user enters 0,
      -> !                                          % then cut (!) to stop the loop and the predicate.
      ; countdown(N)                                % Else, call the countdown predicate with the input number.
    ),
    nl.                                             % Print a newline after the countdown is complete.

% Usage
% ?- nested_countdown.