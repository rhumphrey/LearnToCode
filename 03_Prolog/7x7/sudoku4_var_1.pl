% 6 by 6 puzzles, with 3 by 2 squares
% Check if all elements in a list are different
all_different([]).
all_different([Head|Tail]) :-
    % Ensure that Head is not a member of the Tail (i.e., different from other elements)
    \+ member(Head, Tail),
    % Recursively check the rest of the list
    all_different(Tail).

% Check if all elements in a list are within the domain (1 to 6 for a six-by-six Sudoku)
within_domain([]).
within_domain([Head|Tail]) :-
    % Ensure that Head is a member of the domain [1, 2, 3, 4, 5, 6]
    member(Head, [1, 2, 3, 4, 5, 6]),
    % Recursively check the rest of the list
    within_domain(Tail).

% Validate that each row, column, and three-by-two square has all different values and is within the domain
valid([]).
valid([Head|Tail]) :-
    % Check if all elements in the Head list are different
    all_different(Head),
    % Recursively validate the rest of the rows/columns
    valid(Tail).

% Sudoku solver for six-by-six puzzles with three-by-two squares
sudoku(Puzzle, Solution) :-
    % Ensure that Solution is the same as Puzzle
    Solution = Puzzle,
    % Define the structure of the Puzzle grid
    Puzzle = [S11, S12, S13, S14, S15, S16,
              S21, S22, S23, S24, S25, S26,
              S31, S32, S33, S34, S35, S36,
              S41, S42, S43, S44, S45, S46,
              S51, S52, S53, S54, S55, S56,
              S61, S62, S63, S64, S65, S66],
    % Ensure all elements in the Solution are within the domain
    within_domain(Solution),
    % Define Rows, Columns, and Squares
    Row1 = [S11, S12, S13, S14, S15, S16],
    Row2 = [S21, S22, S23, S24, S25, S26],
    Row3 = [S31, S32, S33, S34, S35, S36],
    Row4 = [S41, S42, S43, S44, S45, S46],
    Row5 = [S51, S52, S53, S54, S55, S56],
    Row6 = [S61, S62, S63, S64, S65, S66],
    Col1 = [S11, S21, S31, S41, S51, S61],
    Col2 = [S12, S22, S32, S42, S52, S62],
    Col3 = [S13, S23, S33, S43, S53, S63],
    Col4 = [S14, S24, S34, S44, S54, S64],
    Col5 = [S15, S25, S35, S45, S55, S65],
    Col6 = [S16, S26, S36, S46, S56, S66],
    Square1 = [S11, S12, S21, S22, S31, S32],
    Square2 = [S13, S14, S23, S24, S33, S34],
    Square3 = [S41, S42, S51, S52, S61, S62],
    Square4 = [S43, S44, S53, S54, S63, S64],
    Square5 = [S15, S16, S25, S26, S35, S36],
    Square6 = [S45, S46, S55, S56, S65, S66],
    % Validate Rows, Columns, and Squares
    valid([Row1, Row2, Row3, Row4, Row5, Row6,
           Col1, Col2, Col3, Col4, Col5, Col6,
           Square1, Square2, Square3, Square4, Square5, Square6]).
