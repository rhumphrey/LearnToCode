list_member(X, [X|_]).
list_member(X, [_|Tail]) :- list_member(X, Tail).

list_length([], 0).
list_length([_|Tail], N) :-
    list_length(Tail, N1),
    N is N1 + 1.

concatenate([], L, L).
concatenate([Head|Tail1], L2, [Head|Tail3]) :-
    concatenate(Tail1, L2, Tail3).

insert_at(Element, List, Position, NewList) :-
    insert_at(Element, List, Position, NewList, 1).

insert_at(Element, [Head|Tail], Position, [Head|NewTail], Counter) :-
    Counter \= Position,
    NewCounter is Counter + 1,
    insert_at(Element, Tail, Position, NewTail, NewCounter).

insert_at(Element, Tail, Position, [Element|Tail], Position).

delete(Element, [Element|Tail], Tail).
delete(Element, [Head|Tail], [Head|NewTail]) :-
    delete(Element, Tail, NewTail).
delete(_, [], []).

% Query Examples
% ?- list_member(green, [red, green, blue]).
% ?- list_length([red, green, blue], Length).
% ?- concatenate([red, green], [blue, white], NewList).
% ?- insert_at(yellow, [red, green, blue], 2, NewList).
% ?- delete(green, [red, green, blue, green], NewList).
