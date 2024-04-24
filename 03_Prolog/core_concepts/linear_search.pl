% Define the base case for an empty list.
linear_search(_, [], _) :- fail.

% If the element is found at the head of the list, return true.
linear_search(Element, [Element|_], 0).

% Otherwise, recursively search the rest of the list.
linear_search(Element, [_|Rest], Index) :-
    linear_search(Element, Rest, IndexRest),
    Index is IndexRest + 1.

% Query to test:
% ?- linear_search(3, [1, 2, 3, 4, 5], Index).
% Output: Index = 2