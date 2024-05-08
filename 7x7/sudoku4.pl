
% Code refactored not to use built-in predicate fd_domain and fd_all_different in GNU-Prolog to make it portable to SWI-Prolog

% Check if all elements in a list are different
all_different([]).
all_different([Head|Tail]) :-
    % Ensure that Head is not a member of the Tail (i.e., different from other elements)
    \+ member(Head, Tail),
    % Recursively check the rest of the list
    all_different(Tail).

% Check if all elements in a list are within the domain (1 to 4 for Sudoku)
within_domain([]).
within_domain([Head|Tail]) :-
    % Ensure that Head is a member of the domain [1, 2, 3, 4]
    member(Head, [1, 2, 3, 4]),
    % Recursively check the rest of the list
    within_domain(Tail).

% Validate that each row and column has all different values and is within the domain
valid([]).
valid([Head|Tail]) :-
    % Check if all elements in the Head list are different
    all_different(Head),
    % Recursively validate the rest of the rows/columns
    valid(Tail).

% Sudoku solver without fd_domain and fd_all_different but using within_domain and all_different predicates defined previously
sudoku(Puzzle, Solution) :-
    % Ensure that Solution is the same as Puzzle
    Solution = Puzzle,
    % Define the structure of the Puzzle grid
    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],
    % Ensure all elements in the Solution are within the domain
    within_domain(Solution),
    % Define Rows, Columns, and Squares
    Row1 = [S11, S12, S13, S14],
    Row2 = [S21, S22, S23, S24],
    Row3 = [S31, S32, S33, S34],
    Row4 = [S41, S42, S43, S44],
    Col1 = [S11, S21, S31, S41],
    Col2 = [S12, S22, S32, S42],
    Col3 = [S13, S23, S33, S43],
    Col4 = [S14, S24, S34, S44],
    Square1 = [S11, S12, S21, S22],
    Square2 = [S13, S14, S23, S24],
    Square3 = [S31, S32, S41, S42],
    Square4 = [S33, S34, S43, S44],
    % Validate Rows, Columns, and Squares
    valid([Row1, Row2, Row3, Row4,
           Col1, Col2, Col3, Col4,
           Square1, Square2, Square3, Square4]).