% Calculate the sum of proper divisors of a number
% N is the number for which we want to find the sum of proper divisors.
% Sum will hold the result.
sum_of_divisors(N, Sum) :-
    % findall/3 is a built-in predicate that finds all solutions to a goal and returns a list of these solutions.
    % Here, it finds all Divisors of N that are not equal to N itself (proper divisors).
    findall(Div, (between(1, N, Div), N mod Div =:= 0, Div \= N), Divisors),
    % sum_list/2 is a built-in predicate that sums all elements of a list.
    % Here, it sums all proper divisors of N to get the Sum.
    sum_list(Divisors, Sum).

% Generate the aliquot sequence
% N is the starting number of the sequence.
% Seq will be the resulting aliquot sequence as a list.
aliquot_sequence(1, [1]) :- !.  % The sequence terminates with 1, so we return a list with only 1.
aliquot_sequence(N, [N|Seq]) :-
    % Calculate the sum of proper divisors of N.
    sum_of_divisors(N, Next),
    % Recursively call aliquot_sequence/2 with the sum of divisors to generate the rest of the sequence.
    aliquot_sequence(Next, Seq).

% Helper predicate to print the sequence
% N is the number from which to start the sequence.
print_aliquot_sequence(N) :-
    % Generate the aliquot sequence starting from N.
    aliquot_sequence(N, Seq),
    % writeln/1 is a built-in predicate that prints the argument followed by a newline.
    % Here, it prints the generated aliquot sequence.
    writeln(Seq).

% Example query to generate and print the sequence
% ?- print_aliquot_sequence(12).
% This query will print the aliquot sequence starting from 12.
