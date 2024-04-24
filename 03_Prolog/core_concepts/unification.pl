% Define two facts
parent(alan, clive).
parent(brian, dave).

% Query with unification
% ?- parent(X, clive).

% In this example:
% We have two facts defined in the knowledge base.
%   The query ?- parent(X, clive). attempts to unify with the facts.
%   Prolog will unify X with alan because it matches the fact parent(alan, clive).
%   The variable X is now bound to the value alan, making the terms identical.