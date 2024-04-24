write_to_file(File, Text) :-
    open(File, write, Stream), % Open the file in write mode, creating a stream.
    write(Stream, Text),       % Write the text to the stream.
    close(Stream).             % Close the stream, which also closes the file.

% Query:
% write_to_file('file.txt', 'Hello, World!').