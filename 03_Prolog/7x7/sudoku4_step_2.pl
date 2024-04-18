% Code refactored not to use built-in predicate fd_domain in GNU-Prolog to make it portable to SWI-Prolog

% The main predicate for solving Sudoku puzzles.
sudoku(Puzzle, Solution) :-
    % Ensure that Solution is the same as Puzzle.
    Solution = Puzzle,
    % Define the structure of the Puzzle grid.
    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],
    
    % Validate that all elements in the Puzzle are within the domain [1, 2, 3, 4].
    within_domain([S11, S12, S13, S14,
                   S21, S22, S23, S24,
                   S31, S32, S33, S34,
                   S41, S42, S43, S44]).

% Predicate to check if all elements in a list are within the specified domain.
within_domain([]).
within_domain([Head|Tail]) :-
    % Ensure that Head is a member of [1, 2, 3, 4].
    member(Head, [1, 2, 3, 4]),
    % Recursively check the rest of the list.
    within_domain(Tail).