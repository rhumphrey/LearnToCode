-module(macro_example).
-define(HELLO_WORLD, io:format("Hello, World!~n")).
-export([run/0]).

run() ->
    ?HELLO_WORLD.