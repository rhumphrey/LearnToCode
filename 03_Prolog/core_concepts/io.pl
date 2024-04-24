ask_number :-
    write('Enter a number: '),          % is used to prompt the user.
    flush_output(current_output),       % ensures that the prompt is displayed before reading the input.
    read(Number),                       % reads the user’s input.
    write('You entered: '),             
    writeln(Number).                    % is similar to write/1 but also adds a newline after the output.
