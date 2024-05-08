% Simple chatbot rules
response(hello, 'Hi there!').
response(hi, 'Hello!').
response(goodbye, 'Goodbye!').
response(bye, 'See you later!').

% Main chatbot predicate
chatbot :-
    writeln('quit. to exit chat'),
    repeat,
    write('You: '),                                         % Prompt the user for input
    read(UserInput),                                        % Read the user's input
    (UserInput = quit ; process_input(UserInput)),          % If input is "quit," exit; otherwise, process it
    !.                                                      % Cut to prevent backtracking

% Process user input
process_input(UserInput) :-
    response(UserInput, BotResponse),                       % Look up the user input in the response rules
    format('Chatbot: ~w~n', [BotResponse]),                 % Print the chatbot's response
    fail.                                                   % Force backtracking to continue the conversation loop

% Start the chatbot immediately when the file is loaded
:- initialization(chatbot).