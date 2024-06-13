-module(translate).                                 % Defines the module name as 'translate'.
-export([loop/0]).                                  % Exports the function 'loop' with 0 arguments, making it callable from outside the module.

loop() ->                                           % Defines the function 'loop'.
    receive                                         % Starts a receive block to wait for messages.
        "casa" ->                                   % If the received message is "casa",
            io:format("house~n" ),                  % it prints "house" followed by a newline character.
            loop();                                 % Calls itself recursively to continue the loop.
        "blanca" ->                                 % If the received message is "blanca",
            io:format("white~n" ),                  % it prints "white" followed by a newline character.
            loop();                                 % Calls itself recursively to continue the loop.
        _ ->                                        % If the received message is anything else,
            io:format("I don't understand.~n" ),    % it prints "I don't understand." followed by a newline character.
            loop()                                  % Calls itself recursively to continue the loop.
    end.                                            % Ends the receive block.
