% Base case: If the list has only one element, that element is the smallest.
smallest([Min], Min).

% Recursive case: To find the smallest element in a list,
% first find the smallest in the tail, then compare it with the head.
smallest([Head|Tail], Min) :-
    smallest(Tail, TailMin),            % Find the smallest in the tail
    Min is min(Head, TailMin).          % The smallest is the minimum of Head and TailMin