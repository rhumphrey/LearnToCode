% Predicate to check if three sides form a triangle
valid_triangle(Side1, Side2, Side3) :-
    Side1 + Side2 > Side3,
    Side1 + Side3 > Side2,
    Side2 + Side3 > Side1.

% Predicates to determine the type of triangle
triangle_type(Side1, Side2, Side3, equilateral) :-
    Side1 == Side2, Side2 == Side3.

triangle_type(Side1, Side2, Side3, isosceles) :-
    (Side1 == Side2; Side1 == Side3; Side2 == Side3) ; 
    (triangle_type(Side1, Side2, Side3, equilateral)).

triangle_type(Side1, Side2, Side3, scalene) :-
    Side1 \= Side2, Side1 \= Side3, Side2 \= Side3.

% Main predicate to identify if sides form a valid triangle and its type
triangle(Side1, Side2, Side3, Type) :-
    valid_triangle(Side1, Side2, Side3),
    triangle_type(Side1, Side2, Side3, Type).