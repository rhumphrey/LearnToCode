% Base case: The reverse of an empty list is an empty list.
reverse_list([], []).

% Recursive case: To reverse a list, first reverse the tail of the list,
% then append the head to the reversed tail.
reverse_list([Head|Tail], Reversed) :-
    reverse_list(Tail, RevTail),                        % Reverse the tail of the list
    append(RevTail, [Head], Reversed).                  % Append the head to the reversed tail