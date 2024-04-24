% The standard cut in Prolog, represented by the exclamation mark !, is used to control backtracking. 
% When Prolog encounters a cut in a clause, it will not backtrack past that point in the current clause 
% if a failure occurs later on. This can be used to improve efficiency by preventing unnecessary backtracking 
% or to enforce determinism in the logic.

% Define a predicate max/3 that determines the maximum of two numbers
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% Example usage
% ?- max(10, 3, Max).
% Max = 10.

% The first clause checks if X is greater than or equal to Y. 
% If this is true, X is the maximum, and the cut (!) is encountered. 
% The cut prevents Prolog from considering the second clause if the first clause succeeds, thus making the decision deterministic.
% The second clause is only considered if the first clause fails (i.e., if X is less than Y). In this case, Y is the maximum.
% The cut is particularly useful when you have mutually exclusive conditions and you want to avoid unnecessary checks once a condition is met. 
% It’s a powerful tool but should be used with care, as it can make the logic less clear and more difficult to maintain. 
% Always consider if there’s a way to write your predicates that avoids the need for cuts, maintaining the declarative nature of Prolog.