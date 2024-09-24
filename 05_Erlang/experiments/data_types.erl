-module(data_types).
-export([start/0]).

start() ->
   io:fwrite("~w~n", [1+1]),            % Number - used to represent an integer or a float
   io:fwrite("~w~n", [atom1]),          % Atom - a literal, a constant with name.   
   io:fwrite("~w~n", [2 =< 3]),         % Boolean - represents a Boolean value which can either be true or false
   Bin1 = <<10,20>>,                    % Bit String - used to store an area of un-typed memory
   X = binary_to_list(Bin1),            % Convert a Bit String to a list X
   io:fwrite("~w~n",[X]),               % Display X
   P = {john,24,{june,25}} ,            % Tuple - compound data type with a fixed number of terms
   io:fwrite("~w~n",[tuple_size(P)]),   % Display size of P
   M1 = #{name=>john,age=>25},          % Map - compound data type with a variable number of key-value associations
   io:fwrite("~w~n",[map_size(M1)]),    % Dispaly size of M1
   L = [10,20,30],                      % List - compound data type with a variable number of terms
   io:fwrite("~w~n",[length(L)]).       % Display length of L
   