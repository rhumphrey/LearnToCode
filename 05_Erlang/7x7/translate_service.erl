-module(translate_service).                     % Defines the module name as 'translate_service'.
-export([loop/0, translate/2]).                 % Exports two functions: 'loop' with 0 arguments and 'translate' with 2 arguments.

loop() ->                                       % Defines the function 'loop'.
    receive                                     % Starts a receive block to wait for messages.
        {From, "casa"} ->                       % If the received message is a tuple with 'From' (the sender's process ID) and the string "casa",
            From ! "house",                     % it sends back the translation "house" to the sender.
            loop();                             % Calls itself recursively to continue the loop.
        {From, "blanca"} ->                     % If the received message is a tuple with 'From' and the string "blanca",
            From ! "white",                     % it sends back the translation "white" to the sender.
            loop();                             % Calls itself recursively to continue the loop.
        {From, _} ->                            % If the received message is a tuple with 'From' and any other string,
            From ! "I don't understand.",       % it sends back the message "I don't understand." to the sender.
            loop()                              % Calls itself recursively to continue the loop.
    end.                                        % Ends the receive block.

translate(To, Word) ->                          % Defines the function 'translate' with arguments 'To' (the recipient's process ID) and 'Word' (the word to be translated).
    To ! {self(), Word},                        % Sends a message to the recipient consisting of the current process ID and the word to be translated.
    receive                                     % Starts a receive block to wait for the translation.
        Translation -> Translation              % Returns the received translation.
    end.                                        % Ends the receive block.