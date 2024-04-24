% Using the -> Operator for If-Then-Else: 
% The -> operator is used in Prolog for if-then-else constructs. 
% The syntax is (Condition -> ThenClause ; ElseClause). 
% If the Condition is true, the ThenClause is executed; otherwise, the ElseClause is executed.

check_number(X) :-
    (   X < 0 -> writeln('X is negative.'),
        fail
    ;   X =:= 0 -> writeln('X is zero.')
    ;   writeln('X is positive.')
    ).


% Using the ; Operator for Disjunction: The ; operator represents a logical OR. 
% It can be used to provide alternative solutions or paths in the program.

food_type(Fruit, fruit) :- Fruit = apple; Fruit = pear.
food_type(Vegetable, vegetable) :- Vegetable = broccoli; Vegetable = spinach.

% Query
% ?- food_type(apple, What).  
% What = fruit ;
% false.


% Soft Cut (*->): The soft cut, denoted by *->, is similar to the standard cut but allows for backtracking. 
% The syntax is (Condition *-> ThenClause ; ElseClause). If Condition succeeds, ThenClause is executed; 
% if Condition fails, ElseClause is executed. 
% Unlike the standard cut, if ThenClause fails, Prolog will backtrack and try ElseClause.

max(X, Y, Max) :-
    (   X >= Y
    *-> Max = X
    ;   Max = Y
    ).

% Query
% ?- max(10, 3, Max).
% Max = 10.