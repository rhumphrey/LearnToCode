-module(count).
-export([count_words/1]).
-export([count_to_ten/0]).

% Function to count words in a string
count_words(String) ->
    % Split the string into words and process the list
    Words = string:split(String, " ", all),
    % Check if the list is a single empty string
    if Words == [""] -> 0;                % Return 0 for an empty string
       true -> count_words_list(Words)    % Otherwise, process the list
    end.

% Helper function to process the list of words
count_words_list([]) -> 0;                                       % Base case: empty list returns 0
count_words_list([_Head | Tail]) -> 1 + count_words_list(Tail).  % Recursive case: count the head and proceed with the tail



% Function to start the count
count_to_ten() ->
    count_up(1).

% Recursive function to count up to ten
count_up(10) ->
    io:format("10~n");                      % Print 10 and stop the recursion
count_up(N) when N < 10 ->
    io:format("~p~n", [N]),                 % Print the current number
    count_up(N + 1).                        % Recurse with the next number
