-module(roulette).                                  % Defines the module name as 'roulette'.
-export([loop/0]).                                  % Exports the function 'loop' with 0 arguments, making it callable from outside the module.

                                                    % The comment indicates that a number between 1 and 6 should be sent to this process.
loop() ->                                           % Defines the function 'loop'.
    receive                                         % Starts a receive block to wait for messages.
        3 ->                                        % If the received message is the number 3,
            io:format("bang.~n"),                   % it prints "bang." followed by a newline character.
            exit({roulette,die,at,erlang:time()});  % Then it terminates the process with a tuple containing the reason for termination and the current time.
        _ ->                                        % If the received message is any other number,
            io:format("click~n"),                   % it prints "click" followed by a newline character.
            loop()                                  % Calls itself recursively to continue the loop.
    end.                                            % Ends the receive block.
