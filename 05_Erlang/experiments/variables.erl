-module(variables). 
-export([start/0]).

%% All variables are bound with the ‘=’ statement
%% All variables need to start with the upper case character
%% All variables are immutable, so can only be assigned once

start() -> 
   X = 20,                          % X is bound to 20 (integer)
   Y = 30,                          % Y is bound to 30 (integer)
   Z = 40.00,                       % Z is bound to 40.00 (float)
   Result = X + Y,                  % Result is bound to X + Y
   io:fwrite("~w~n",[Result]),      % Output integer Result
   io:fwrite("~f~n",[Z]),           % Output float Z with the default precision of 6 written as [-]ddd.ddd
   io:fwrite("~e~n",[Z]).           % Output float Z with the default precision of 6 written as [-]d.ddde+-ddd