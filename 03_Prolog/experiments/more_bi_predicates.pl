/*
These examples show how Prolog predicates can be used for various tasks. Feel free to experiment with your own data and queries.
By moving the predicates and arguments into the knowledge base as facts and rules and creating further predicates rules to act as wrappers for queries
You can try these out as queries with no knowledge base loaded just copy the examples after the ?- into the ?- prompt in SWI-Prolog

1. `member/2`: Checks if an element is a member of a list.
    
    % Example: Is 3 a member of the list [1, 2, 3, 4]?
    ?- member(3, [1, 2, 3, 4]).
    

2. `append/3`: Concatenates two lists or adds an element to a list.
    
    % Example: Concatenate two lists [1, 2] and [3, 4] into Result.
    ?- append([1, 2], [3, 4], Result).
    

3. `select/3`: Selects an element from a list and provides the rest of the list.
    
    % Example: Select an element from [a, b, c] and get the remaining list.
    ?- select(b, [a, b, c], Rest).
    

4. `length/2`: Determines the length of a list.
    
    % Example: Calculate the length of [apple, banana, cherry].
    ?- length([apple, banana, cherry], Len).
    

5. `nth0/3` and `nth1/3`: Retrieves an element based on its index in a list (0-based or 1-based).
    
    % Example: Get the 2nd element (1-based) from [a, b, c].
    ?- nth1(2, [a, b, c], Element).
    

6. `findall/3`: Collects all solutions to a query and returns them in a list.
    
    % Example: Find all even numbers between 1 and 10.
    ?- findall(X, (between(1, 10, X), X mod 2 =:= 0), Evens).
    

7. `setof/3`: Sorts a list and removes duplicates.
    
    % Example: Set of unique colors from a list.
    ?- setof(Color, member(Color, [red, green, blue, red]), UniqueColors).
    

8. `delete/3`: Removes elements from a list that match a given pattern.
    
    % Example: Remove all occurrences of 'apple' from [apple, banana, apple].
    ?- delete([apple, banana, apple], apple, Result).
    

9. `nextto/3`: Checks if one element follows another in a list.
    
    % Example: Is 'b' next to 'a' in [a, b, c]?
    ?- nextto(a, b, [a, b, c]).

*/