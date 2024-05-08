/*
1. square_list(InputList, SquaredList)
    - This predicate defines a relationship between an input list (`InputList`) and a squared list (`SquaredList`).
    - It uses the `maplist/3` predicate to apply the `square/2` predicate to each element of `InputList` and store the results in `SquaredList`.

2. square(X, Y)
    - This predicate defines a relationship between an input value `X` and its square value `Y`.
    - It calculates the square of `X` by multiplying it with itself and assigns the result to `Y`.

3. nested_square_list(InputLists, SquaredLists)
    - This predicate relates a list of input lists (`InputLists`) to a list of squared lists (`SquaredLists`).
    - It uses `maplist/3` to apply the `square_list/2` predicate to each sublist within `InputLists`.

4. zip_lists(Names, Values, Zipped)
    - This predicate combines two lists: a list of names (`Names`) and a list of corresponding values (`Values`).
    - It creates a new list `Zipped` where each element is a pair of a name and its corresponding value (e.g., `"John-42"`).
    - maplist/4:
      The maplist/4 predicate is used to apply a given predicate to corresponding elements from multiple lists.
      In this case, it takes four arguments: the predicate [N, V, N-V] >> true, the Names list, the Values list, and the resulting Zipped list.
    - [N, V, N-V] >> true:
      This is an anonymous predicate (also known as a lambda expression) that operates on three arguments: N, V, and N-V.
      The >> operator separates the input arguments from the body of the lambda expression.
      What happens inside the lambda expression:
      - N represents an element from the Names list.
      - V represents the corresponding element from the Values list.
      - N-V constructs a pair where N is the key (name) and V is the value.
      - The true at the end of the lambda expression ensures that the mapping is successful (i.e., it always returns true).
*/

% Squares each element in the list using maplist
square_list(InputList, SquaredList) :-
    maplist(square, InputList, SquaredList).
square(X, Y) :-
    Y is X * X.
% Query:
% ?- square_list([2, 4, 6], SquaredList).
% SquaredList = [4, 16, 36].



% Applies square_list to each sublist
nested_square_list(InputLists, SquaredLists) :-
    maplist(square_list, InputLists, SquaredLists).
% Query:
% ?- nested_square_list([[1, 2], [3, 4]], NestedSquared).
% NestedSquared = [[1, 4], [9, 16]].



% Combines corresponding elements from Names and Values using maplist
zip_lists(Names, Values, Zipped) :-
    maplist([N, V, N-V] >> true, Names, Values, Zipped).
% Query:
% ?- zip_lists([alice, bob, carol], [apple, banana, cherry], Zipped).
% Zipped = [alice-apple, bob-banana, carol-cherry].
