% Paths in a directed graph a -> b, b -> c, c -> d, d -> e
% Facts representing the directed edges
edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).

% Recursive predicate to find paths
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% The edge/2 facts define the directed edges between nodes.
% The path/2 predicate recursively finds paths from X to Y.
% If there’s a direct edge from X to Y, it’s a valid path.
% Otherwise, it checks if there’s an intermediate node Z such that there’s an edge from X to Z and a path from Z to Y.

% Query 
% ?- path(a, e).