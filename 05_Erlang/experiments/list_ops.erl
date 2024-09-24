-module(list_ops).
-export([reverse/1, length/1, sum/1, member/2]).

% Reverse a list
reverse(List) -> lists:reverse(List).

% Calculate the length of a list
length(List) -> length(List, 0).

length([], Acc) -> Acc;
length([_ | Tail], Acc) -> length(Tail, Acc + 1).

% Sum the elements of a list
sum(List) -> sum(List, 0).

sum([], Acc) -> Acc;
sum([Head | Tail], Acc) -> sum(Tail, Acc + Head).

% Check if an element is a member of the list
member(Element, List) -> lists:member(Element, List).
