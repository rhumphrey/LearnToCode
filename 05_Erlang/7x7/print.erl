-module(print).
-export([print_message/1]).

% Function to print messages based on input
print_message(success) ->
    io:format("success~n");
print_message({error, Message}) ->
    io:format("error: ~p~n", [Message]).