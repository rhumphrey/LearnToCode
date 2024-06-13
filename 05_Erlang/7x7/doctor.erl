-module(doctor).                                                    % Defines the module name as 'doctor'.
-export([loop/0]).                                                  % Exports the function 'loop' with 0 arguments, making it callable from outside the module.

loop() ->                                                           % Defines the function 'loop'.
    process_flag(trap_exit, true),                                  % Sets the process flag to trap exit signals, allowing the process to handle exit signals sent by terminating processes it's linked to.
    receive                                                         % Starts a receive block to wait for messages.
        new ->                                                      % If the received message is 'new',
            io:format("Creating and monitoring process.~n"),        % it prints a message about creating and monitoring a new process.
            register(revolver, spawn_link(fun roulette:loop/0)),    % It registers a new process with the name 'revolver', which is linked to the current process and starts executing the 'loop' function from the 'roulette' module.
            loop();                                                 % Calls itself recursively to continue the loop.
        
        {'EXIT', From, Reason} ->                                   % If the received message is an exit signal,
            io:format("The shooter ~p died with reason ~p.", [From, Reason]), % it prints a message about the termination of the linked process, including the process identifier and the reason for termination.
            io:format(" Restarting. ~n"),                           % It prints a message about restarting.
            self() ! new,                                           % It sends a 'new' message to itself, effectively restarting the process creation and monitoring.
            loop()                                                  % Calls itself recursively to continue the loop.
    end.                                                            % Ends the receive block.
