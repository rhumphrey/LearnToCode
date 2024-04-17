% insert predicate

% Base case: Inserting X into an empty list results in a list with just X.
insert(X, [], [X]).

% Recursive case 1: If X is less than or equal to the head of the list (Y),
% X is inserted before Y.
insert(X, [Y|Tail], [X,Y|Tail]) :- X =< Y.

% Recursive case 2: If X is greater than the head of the list (Y),
% the function continues to try inserting X into the tail of the list.
insert(X, [Y|Tail], [Y|NewTail]) :- X > Y, insert(X, Tail, NewTail).



% insertion_sort predicate

% Base case: An empty list is already sorted.
insertion_sort([], []).

% Recursive case: To sort a list, first sort the tail of the list,
% then insert the head of the list into the sorted tail.
insertion_sort([Head|Tail], Sorted) :-
    insertion_sort(Tail, SortedTail),
    insert(Head, SortedTail, Sorted).