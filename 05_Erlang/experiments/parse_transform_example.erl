-module(parse_transform_example).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %% Add a function to the module being compiled
    NewFunc = {
        function, 1, hello, 0,
        [{clause, 1, [], [],
          [{atom, 1, hello_world}]}]
    },
    %% Append the new function to the existing forms
    Forms ++ [NewFunc].