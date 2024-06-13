-module(coroner).                                                               % Defines the module name as 'coroner'.
-export([loop/0]).                                                              % Exports the function 'loop' with 0 arguments, making it callable from outside the module.

loop() ->                                                                       % Defines the function 'loop'.
    process_flag(trap_exit, true),                                              % Sets the process flag to trap exit signals, allowing the process to handle exit signals sent by terminating processes it's linked to.
    receive                                                                     % Starts a receive block to wait for messages.
        {monitor, Process} ->                                                   % If the received message is a tuple with 'monitor' and a process identifier,
            link(Process),                                                      % it creates a link to the specified process.
            io:format("Monitoring process.~n"),                                 % Prints a message indicating it's monitoring the process.
            loop();                                                             % Calls itself recursively to continue the loop.
        {'EXIT', From, Reason} ->                                               % If the received message is an exit signal,
            io:format("The shooter ~p died with reason ~p.", [From, Reason]),   % it prints a message about the termination of the monitored process, including the process identifier and the reason for termination.
            io:format("Start another one.~n"),                                  % Prints a message indicating it will start monitoring another process.
            loop()                                                              % Calls itself recursively to continue the loop.
    end.                                                                        % Ends the receive block.
