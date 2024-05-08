% Code refactored not to use built-in predicate all_different in GNU-Prolog to make it portable to SWI-Prolog

% Check if all elements in a list are different
all_different([]).
all_different([Head|Tail]) :-
    \+ member(Head, Tail),  % Ensure that Head is not a member of the Tail (i.e., different from other elements)
    all_different(Tail).    % Recursively check the rest of the list

% Define a valid queen position (Row, Col) on an 8x8 chessboard.
valid_queen((Row, Col)) :-
    member(Col, [1,2,3,4,5,6,7,8]).

% Check if a list of queen positions forms a valid board configuration.
valid_board([]).
valid_board([Head|Tail]) :-
    valid_queen(Head),
    valid_board(Tail).

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
    Board = [(1, _), (2, _), (3, _), (4, _), (5, _), (6, _), (7, _), (8, _)],
    valid_board(Board),
    cols(Board, Cols),
    diags1(Board, Diags1),
    diags2(Board, Diags2),
    all_different(Cols),         % Ensure columns are distinct
    all_different(Diags1),       % Ensure diagonals are distinct
    all_different(Diags2).       % Ensure diagonals are distinct
