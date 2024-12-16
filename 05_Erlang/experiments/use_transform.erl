-module(use_transform).
-compile({parse_transform, parse_transform_example}).
-export([run/0, hello/0]).

run() ->
    io:format("Call parse transformed function: ~p~n", [hello()]).