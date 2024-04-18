% Code refactored not to use built-in predicate all_different in GNU-Prolog to make it portable to SWI-Prolog

% Check if all elements in a list are different
all_different([]).
all_different([Head|Tail]) :-
    \+ member(Head, Tail),  % Ensure that Head is not a member of the Tail (i.e., different from other elements)
    all_different(Tail).    % Recursively check the rest of the list

% Define a valid queen position (Row, Col) on an 8x8 chessboard.
valid_queen((Row, Col)) :-
    Range = [1,2,3,4,5,6,7,8],      % Define the valid range for rows and columns
    member(Row, Range),             % Row must be within the valid range
    member(Col, Range).             % Column must be within the valid range

% Check if a list of queen positions forms a valid board configuration.
valid_board([]).                    % An empty board is valid.
valid_board([Head|Tail]) :-
    valid_queen(Head),              % Check if the head queen is valid
    valid_board(Tail).              % Recursively check the tail of the board

% Extract the rows from a list of queen positions.
rows([], []).
rows([(Row, _)|QueensTail], [Row|RowsTail]) :-
    rows(QueensTail, RowsTail).

% Extract the columns from a list of queen positions.
cols([], []).
cols([(_, Col)|QueensTail], [Col|ColsTail]) :-
    cols(QueensTail, ColsTail).

% Extract the diagonals (difference of column and row) from a list of queen positions.
diags1([], []).
diags1([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
    Diagonal is Col - Row,
    diags1(QueensTail, DiagonalsTail).

% Extract the diagonals (sum of column and row) from a list of queen positions.
diags2([], []).
diags2([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
    Diagonal is Col + Row,
    diags2(QueensTail, DiagonalsTail).

% Solve the eight queens problem.
eight_queens(Board) :-
    length(Board, 8),               % The board must have 8 queens
    valid_board(Board),             % Check if the board configuration is valid
    rows(Board, Rows),
    cols(Board, Cols),
    diags1(Board, Diags1),
    diags2(Board, Diags2),
    all_different(Rows),            % Ensure rows are distinct
    all_different(Cols),            % Ensure columns are distinct
    all_different(Diags1),          % Ensure diagonals are distinct
    all_different(Diags2).          % Ensure diagonals are distinct
