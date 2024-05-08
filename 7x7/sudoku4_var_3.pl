% Check if all elements in a list are different
all_different([]).
all_different([Head|Tail]) :-
    \+ member(Head, Tail),
    all_different(Tail).

% Check if all elements in a list are within the domain (1 to 4 for Sudoku)
within_domain([]).
within_domain([Head|Tail]) :-
    member(Head, [1, 2, 3, 4]),
    within_domain(Tail).

% Validate that each row and column has all different values and is within the domain
valid([]).
valid([Head|Tail]) :-
    all_different(Head),
    valid(Tail).

% Sudoku solver without fd_domain and fd_all_different but using within_domain and all_different predicates defined previously
sudoku(Puzzle, Solution) :-
    Solution = Puzzle,
    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],
    within_domain(Solution),
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
    valid([Row1, Row2, Row3, Row4,
           Col1, Col2, Col3, Col4,
           Square1, Square2, Square3, Square4]),
    % Print the solution in a more readable format
    format_solution([Row1, Row2, Row3, Row4]).

% Format and print the solution
format_solution([]).
format_solution([Row|Rest]) :-
    format("~w ~w ~w ~w~n", Row),
    format_solution(Rest).
