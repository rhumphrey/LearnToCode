-module(tic_tac_toe).      % Defines the module name as tic_tac_toe.
-export([check_winner/1]). % Exports the function check_winner with 1 argument.

% check_winner/1: Determines the winner of the tic-tac-toe game.
check_winner(Board) ->
    % Normalizes the Board input to a list if it's a tuple.
    NormalizedBoard = case Board of
        _ when is_list(Board) -> Board;                  % If Board is already a list, use it as is.
        _ when is_tuple(Board) -> tuple_to_list(Board)   % If Board is a tuple, convert it to a list.
    end,
    determine_winner(NormalizedBoard).                   % Calls determine_winner/1 with the normalized board.

% determine_winner/1: Checks if there's a winner or the game is a draw/cat's game.
determine_winner(Board) ->
    % Checks if player 'x' has won.
    case has_winner(Board, 'x') of
        true -> 'x'; % If 'x' is the winner, return 'x'.
        false ->
            % If 'x' hasn't won, check if player 'o' has won.
            case has_winner(Board, 'o') of
                true -> 'o'; % If 'o' is the winner, return 'o'.
                false ->
                    % If neither 'x' nor 'o' has won, check if there are any empty spaces left.
                    case lists:any(fun(Elem) -> Elem == '_' end, Board) of
                        true -> no_winner;  % If there are empty spaces, the game is still ongoing.
                        false -> cat        % If there are no empty spaces, the game is a draw (cat's game).
                    end
            end
    end.

% has_winner/1: Checks if a specific player has won the game.
has_winner(Board, Player) ->
    % Defines all possible winning lines in the game.
    Lines = [
        [lists:nth(1, Board), lists:nth(2, Board), lists:nth(3, Board)],    % Top row.
        [lists:nth(4, Board), lists:nth(5, Board), lists:nth(6, Board)],    % Middle row.
        [lists:nth(7, Board), lists:nth(8, Board), lists:nth(9, Board)],    % Bottom row.
        [lists:nth(1, Board), lists:nth(4, Board), lists:nth(7, Board)],    % Left column.
        [lists:nth(2, Board), lists:nth(5, Board), lists:nth(8, Board)],    % Center column.
        [lists:nth(3, Board), lists:nth(6, Board), lists:nth(9, Board)],    % Right column.
        [lists:nth(1, Board), lists:nth(5, Board), lists:nth(9, Board)],    % Diagonal from top-left to bottom-right.
        [lists:nth(3, Board), lists:nth(5, Board), lists:nth(7, Board)]     % Diagonal from top-right to bottom-left.
    ],
    % Checks if any line has all elements equal to the Player's symbol ('x' or 'o').
    lists:any(fun(Line) -> lists:all(fun(Elem) -> Elem == Player end, Line) end, Lines).

