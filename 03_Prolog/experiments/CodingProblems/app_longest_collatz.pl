/*
This problem was asked by Apple.
A Collatz sequence in mathematics can be defined as follows. Starting with any positive integer:
•	if n is even, the next number in the sequence is n / 2
•	if n is odd, the next number in the sequence is 3n + 1 
It is conjectured that every such sequence eventually reaches the number 1. Test this conjecture.
Bonus: What input n <= 1000000 gives the longest sequence?
*/

% Predicate to calculate the length of the Collatz sequence
collatz_length(1, 1) :- !. % Base case: sequence for 1 has length 1
collatz_length(N, Length) :-
    N > 1,
    (   0 is N mod 2
    ->  Half is N // 2,
        collatz_length(Half, L),
        Length is L + 1
    ;   Next is 3 * N + 1,
        collatz_length(Next, L),
        Length is L + 1
    ).

% Predicate to find the number with the longest Collatz sequence
longest_collatz(Max, Number, MaxLength) :-
    findall(Length-N, (between(1, Max, N), collatz_length(N, Length)), Lengths),
    max_member(MaxLength-Number, Lengths).

% Example usage:
% ?- longest_collatz(1000000, Number, Length).
% This will find the number less than or equal to 1,000,000 with the longest Collatz sequence and its length.