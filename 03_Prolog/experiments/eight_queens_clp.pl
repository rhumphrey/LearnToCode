% Solves the N-Queens problem by finding a placement of N queens on an NxN chessboard
% such that no two queens threaten each other.

:- use_module(library(clpfd)).                                                          % Include the CLP(FD) library for constraints.
                                                                                        % due to the use of the ins keyword an the #\= operator, 
                                                                                        % which are part of the Constraint Logic Programming over Finite Domains library.

n_queens(BoardSize, QueensPositions) :-                                                 % Initialize the N-Queens problem.
    length(QueensPositions, BoardSize),                                                 % Ensure QueensPositions list has length BoardSize.
    QueensPositions ins 1..BoardSize,                                                   % Constrain QueensPositions elements to 1..BoardSize.
    safe_queens(QueensPositions).                                                       % Check for a safe configuration of queens.

% safe_queens/1: Checks if the current configuration of queens is safe.
safe_queens([]).                                                                        % An empty list is safe as there are no queens to threaten each other.
safe_queens([Queen|OtherQueens]) :-                                                     % Check safety of the first queen against others.
    safe_queens(OtherQueens, Queen, 1),                                                 % Use helper predicate for checking.
    safe_queens(OtherQueens).                                                           % Recursively check for the rest of the queens.

% safe_queens/3: Helper predicate to check safety of queens against each other.
safe_queens([], _, _).                                                                  % Base case: No more queens to check, configuration is safe.
safe_queens([NextQueen|RemainingQueens], CurrentQueen, ColumnDistance) :-               % Check safety of CurrentQueen.
    CurrentQueen #\= NextQueen,                                                         % Ensure CurrentQueen is not in the same row as NextQueen.
    abs(CurrentQueen - NextQueen) #\= ColumnDistance,                                   % Ensure queens are not in the same diagonal.
    NextColumnDistance #= ColumnDistance + 1,                                           % Increment column distance for next check.
    safe_queens(RemainingQueens, CurrentQueen, NextColumnDistance).                     % Recursively check remaining queens.

% Query to solve the 8-Queens problem and label the solution.
% ?- n_queens(8, QueensPositions), label(QueensPositions).                              % Solve for an 8x8 board.