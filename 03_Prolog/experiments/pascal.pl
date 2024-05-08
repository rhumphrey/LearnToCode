% Base case: Pascal's triangle with 0 rows.
pascal(0, []) :- !.                                             % Cut to prevent further backtracking.

% Base case: Pascal's triangle with 1 row.
pascal(1, [[1]]) :- !.                                          % Cut to prevent further backtracking.

% Recursive case: Compute Pascal's triangle for NumRows.
pascal(NumRows, Rows) :-
    NumRows > 1,                                                % Ensure NumRows is greater than 1.
    PrevNumRows is NumRows - 1,                                 % Calculate the previous number of rows.
    pascal(PrevNumRows, PrevRows),                              % Recursively call pascal for one less row.
    last(PrevRows, LastRow),                                    % Get the last row of the previously computed triangle.
    next_row(LastRow, NextRow),                                 % Compute the next row based on the last row.
    append(PrevRows, [NextRow], Rows).                          % Append the new row to the existing triangle.

% Compute the next row in Pascal's triangle.
next_row(LastRow, [1|NextRow]) :-
    next_row_helper(LastRow, NextRow).                          % Helper predicate to compute the next row.

% Helper predicate for next_row, adds 1 at the end.
next_row_helper([_], [1]).                                      % Base case for a single element, add 1 at the end.
next_row_helper([Current, Next|RemainingElements], [Sum|RestOfRow]) :-
    Sum is Current + Next,                                      % Calculate the sum of the current and next elements.
    next_row_helper([Next|RemainingElements], RestOfRow).       % Continue with the remaining elements.

% Helper predicate to append an element to a list.
append([], List, List).                                         % Base case for appending an empty list.
append([Head|Tail], List, [Head|ResultingList]) :-
    append(Tail, List, ResultingList).                          % Recursively append the rest of the list.

% Helper predicate to get the last element of a list.
last([Element], Element) :- !.                                  % Base case for a single-element list.
last([_|Tail], Element) :-
    last(Tail, Element).                                        % Recursively find the last element in the list.

% Query example
% ?- pascal(2, Rows)