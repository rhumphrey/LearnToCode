% Define the words for each digit
digit_word(0, zero).
digit_word(1, one).
digit_word(2, two).
digit_word(3, three).
digit_word(4, four).
digit_word(5, five).
digit_word(6, six).
digit_word(7, seven).
digit_word(8, eight).
digit_word(9, nine).

% Convert a single digit number to its word representation
full_words(N) :-
    digit_word(N, Word),
    write(Word).

% Convert a multi-digit number to its word representation
full_words(N) :-
    N >= 10,
    Prev is N // 10,            % Get the previous part of the number
    LastDigit is N mod 10,      % Get the last digit
    full_words(Prev),           % Recursively process the previous part
    write('-'),
    full_words(LastDigit).      % Process the last digit

% Example Usage
% full_words(175).