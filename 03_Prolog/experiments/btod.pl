% Predicate to convert a character to its integer value.
char_to_int('0', 0).                            % Converts character '0' to integer 0.
char_to_int('1', 1).                            % Converts character '1' to integer 1.

% Predicate to reverse a list.
reverse_list([], []).                                   % Base case: an empty list is reversed to an empty list.
reverse_list([Head|Tail], Reversed) :-                  % Takes a list with Head and Tail,
    reverse_list(Tail, RevTail),                        % Recursively reverses the Tail of the list,
    append(RevTail, [Head], Reversed).                  % and appends the Head to the reversed Tail.

% Predicate to convert a list of binary digits to decimal.
binary_list_to_decimal(BinaryList, Decimal) :-          % Takes a BinaryList,
    reverse_list(BinaryList, Reversed),                 % reverses the list to process least significant bit first,
    binary_list_to_decimal(Reversed, 0, 0, Decimal).    % and calls the helper predicate to calculate the decimal value.

% Helper predicate to process each binary digit.
binary_list_to_decimal([], _, Decimal, Decimal).        % Base case: when the list is empty, return the accumulated Decimal value.
binary_list_to_decimal([Head|Tail], Index, Accumulator, Decimal) :- % For each element in the list,
    char_to_int(Head, Digit),                           % convert the character to an integer Digit,
    Value is Digit * 2^Index,                           % calculate the Value of the digit based on its position (Index),
    NewAccumulator is Accumulator + Value,              % add the Value to the Accumulator,
    NewIndex is Index + 1,                              % increment the Index for the next position,
    binary_list_to_decimal(Tail, NewIndex, NewAccumulator, Decimal). % and continue processing the rest of the list.

% Main predicate to convert a binary string to decimal.
binary(Str, Dec) :-                                     % Takes a binary string Str,
    string_chars(Str, CharList),                        % converts it to a list of characters,
    binary_list_to_decimal(CharList, Dec).              % and calls the predicate to convert the list of binary digits to a decimal number.


% Example Usage:
% ?- binary('101010', Decimal).
% Result: Decimal = 42.