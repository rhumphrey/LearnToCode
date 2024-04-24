/*
This problem was asked by Amazon [Easy].
Given an array and a number k that's smaller than the length of the array, rotate the array to the right k elements in-place.
*/


% Base case: If k is 0, the list remains unchanged.
rotate_right(L, 0, L).

% Recursive case: Rotate the list by one element and then rotate k-1 times.
rotate_right(L, K, R) :-
    K > 0,
    K1 is K - 1,
    rotate_once(L, L1),
    rotate_right(L1, K1, R).

% Helper predicate to rotate the list by one element to the right.
rotate_once([H|T], R) :-
    append(T, [H], R).

% Predicate to handle the case when k is greater than the length of the list.
rotate_right(L, K, R) :-
    length(L, Len),
    K > Len,
    K1 is K mod Len,
    rotate_right(L, K1, R).


% Query
% ?- rotate_right([1,2,3,4,5], 2, R).
% Output: R = [3,4,5,1,2].