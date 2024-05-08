% Chatbot main loop
chatbot :-
    % Print a welcome message and prompt the user for input
    write('Welcome to the Prolog chatbot. Type your message:'), nl,
    % Read the user's input as a string
    read_line_to_string(user_input, Input),
    % Process the user's input
    process_input(Input).

% Process user input
process_input("quit") :-
    % If the user types "quit", terminate the chatbot
    write('Chatbot terminated.'), nl.

process_input(Input) :-
    % Split the user's input into words
    split_string(Input, " ", "", Words),
    % Match the words with a predefined response
    match_response(Words, Resp),
    % Print the response
    write(Resp), nl,
    % Continue the chatbot loop
    chatbot.

process_input(_) :-
    % If the input is not understood, ask the user to rephrase
    write('I am not sure I understand. Can you try rephrasing?'), nl,
    % Continue the chatbot loop
    chatbot.

% Match words to responses
match_response(["hello"], 'Hi there! How can I help you today?').
match_response(["hi"], 'Hello! What can I do for you?').
match_response(["bye"], 'Goodbye! Have a great day!').
match_response(["goodbye"], 'Farewell! Looking forward to our next chat.').
match_response(["hey", "there"], 'Hey! How can I help.').

% Start the chatbot
start :-
    % Invoke the chatbot main loop
    chatbot.