% Predicate to perform bubble sort
bubble_sort(List, Sorted) :- 
    b_sort(List, [], Sorted).

% Base case: When the input list is empty, return the accumulator as the sorted list
b_sort([], Acc, Acc).

% Recursive case: Process the head of the list (H) and recursively sort the tail (T)
b_sort([H|T], Acc, Sorted) :- 
    bubble(H, T, NT, Max), % Perform a bubble pass
    b_sort(NT, [Max|Acc], Sorted). % Accumulate the sorted elements

% Predicate to perform a single pass of bubble sort
bubble(X, [], [], X). % Base case: Empty list, return X
bubble(X, [Y|T], [Y|NT], Max) :- 
    X > Y, % If X is greater than Y, swap them
    bubble(X, T, NT, Max).
bubble(X, [Y|T], [X|NT], Max) :-
    X =< Y, % If X is less than or equal to Y, keep Y as is
    bubble(Y, T, NT, Max).

% Query to test:
% ?- bubble_sort([4, 2, 6, 3, 1, 5], Sorted).
% Output: Sorted = [1, 2, 3, 4, 5, 6]
