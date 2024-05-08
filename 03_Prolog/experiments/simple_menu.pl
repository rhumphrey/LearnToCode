% This predicate displays a menu and processes the user's choice.
menu :-
    repeat,                                     % Start an infinite loop.
    writeln('1. Calculate cube'),               % Option 1: Calculate the cube of a number.
    writeln('2. Exit'),                         % Option 2: Exit the program.
    write('Enter your choice: '),               % Prompt the user to enter their choice.
    read(Choice),                               % Read the user's input.
    process_menu_choice(Choice).                % Call the predicate to process the input.

% This predicate processes the user's choice from the menu.
process_menu_choice(1) :-                       % If the user chooses option 1:
    write('Enter a number: '),                  % Prompt the user to enter a number.
    read(N),                                    % Read the number.
    Result is N * N * N,                        % Calculate the cube of the number.
    writeln('Cube: '), writeln(Result),         % Display the result.
    nl,                                         % Print a newline.
    menu.                                       % Display the menu again.
    
process_menu_choice(2) :- !.                    % If the user chooses option 2, cut (!) to exit.
