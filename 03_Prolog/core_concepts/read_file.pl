read_file(File) :-
    open(File, read, Stream), % Open the file in read mode, creating a stream.
    repeat,                   % Start a repeat loop, which will run until 'true' is encountered.
    read_line_to_codes(Stream, Codes), % Read a line from the stream as a list of character codes.
    ( Codes == end_of_file -> % Check if the end of the file has been reached.
        true ;                % If it's the end of the file, succeed and exit the loop.
        atom_codes(Line, Codes), % Convert the character codes to an atom representing the line.
        write(Line), nl,      % Write the line to the console and start a new line.
        fail                  % Fail to backtrack and read the next line.
    ),
    close(Stream).            % Close the stream after reading all lines.

% Query:
% read_file('file.txt').
