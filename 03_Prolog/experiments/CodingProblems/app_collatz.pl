/*
This problem was asked by Apple.
A Collatz sequence in mathematics can be defined as follows. Starting with any positive integer:
•	if n is even, the next number in the sequence is n / 2
•	if n is odd, the next number in the sequence is 3n + 1 
It is conjectured that every such sequence eventually reaches the number 1. Test this conjecture.
Bonus: What input n <= 1000000 gives the longest sequence?
*/


% Define the collatz predicate
collatz(1, [1]) :- !. % Base case: sequence ends with 1
collatz(N, [N|Seq]) :- 
    N > 1, % Ensure N is a positive integer
    (   0 is N mod 2
    ->  Half is N // 2, % If N is even, divide it by 2
        collatz(Half, Seq)
    ;   Next is 3 * N + 1, % If N is odd, multiply by 3 and add 1
        collatz(Next, Seq)
    ).

% Predicate to start the Collatz sequence from a given number
start_collatz(N) :-
    collatz(N, Seq), % Generate the Collatz sequence
    write('Collatz sequence for '), write(N), write(': '), writeln(Seq).