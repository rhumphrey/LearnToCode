-module(find_value).
-export([find_value/2]).

% Define the function 'find_value' that takes a list of tuples and a keyword.
find_value(TupleList, Keyword) ->
    % Use the 'lists:keyfind' function to find the tuple.
    case lists:keyfind(Keyword, 1, TupleList) of
        % If the tuple is found, return the value (second element of the tuple).
        {Keyword, Value} -> Value;
        % If the tuple is not found, return 'undefined'.
        false -> undefined
    end.