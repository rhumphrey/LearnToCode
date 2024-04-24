% Facts: A predicate followed by arguments
studies(ted, cs50X).
studies(eva, cs50X).
studies(syd, cs50I).
studies(cal, cs50P).

teaches(yowlie, cs50X).
teaches(socks, cs50I).
teaches(socks, cs50B).
teaches(pink, cs50P).

% Rules: The relationship
professor(Teacher, Student) :- teaches(Teacher, Course), studies(Student, Course).

% Queries / Goals
% ?- studies(ted, What). % What does Ted study?
% ?- professor(socks, Students). % Who are the students of Professor Socks?
% ?- professor(yowlie, Students). % Who are the students of Professor Yowlie?
