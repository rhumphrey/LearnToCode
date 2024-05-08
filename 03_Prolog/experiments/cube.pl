% Predicate 'cube' starts the process and prompts the user for input.
cube :- 
    write('Input number or stop (end with .): '),           % Print message asking for user input.
    read(X),                                                % Read the user's input.
    process_input(X).                                             % Call the 'process' predicate with the user's input.

% Predicate 'process' with 'stop' as the argument ends the recursion.
process_input(stop) :- 
    !.                                                      % The cut operator (!) stops backtracking, effectively ending the program.

% Predicate 'process' for any number N calculates its cube and recurses.
process_input(N) :- 
    Result is N * N * N,                                    % Calculate the cube of N and store it in 'Result'.
    write('The cube of '), write(N),                        % Print the message part "The cube of N".
    write(' is '), write(Result), nl,                       % Print the message part "is Result" and start a new line.
    cube.                                                   % Recurse by calling 'cube' again for the next input.

% Usage to start progeam
% ?- cube.