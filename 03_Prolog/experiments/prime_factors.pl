% Check if a number is a factor of another
is_factor(X, Y) :- 0 is Y mod X.

% Find the smallest factor of a number
smallest_factor(N, F) :- smallest_factor(N, 2, F).
smallest_factor(N, F, F) :- is_factor(F, N).
smallest_factor(N, F, SF) :-
    F * F =< N,
    (   is_factor(F, N)
    ->  SF = F
    ;   NextF is F + 1,
        smallest_factor(N, NextF, SF)
    ).
smallest_factor(N, F, N) :- F * F > N.

% Determine the prime factors of a number
prime_factors(N, L) :- N > 1, prime_factors(N, L, []).
prime_factors(1, L, L).
prime_factors(N, L, Acc) :-
    N > 1,
    smallest_factor(N, F),
    NewN is N // F,
    prime_factors(NewN, L, [F|Acc]).

% Example query
% ?- prime_factors(315, L).