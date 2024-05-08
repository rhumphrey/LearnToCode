% Calculate the sum of a list of numbers
sum_list([], 0).                                            % Base case: the sum of an empty list is 0
sum_list([X | Rest], Sum) :-                                % Recursive case: split the list into head (X) and tail (Rest)
    sum_list(Rest, RestSum),                                % Recursively calculate the sum of the tail
    Sum is X + RestSum.                                     % The sum is the head (X) plus the sum of the tail (RestSum)

% Calculate the average of a list of numbers
average(List, Avg) :- 
    sum_list(List, Sum),                                    % Call sum_list to get the sum of the list
    length(List, Count),                                    % Get the number of elements in the list
    Avg is Sum / Count.                                     % Calculate the average by dividing the sum by the count

% Filter numbers greater than the average
greater_than_average([], _, []).                            % Base case: no numbers are greater than the average in an empty list
greater_than_average([X | Rest], Avg, [X | Greater]) :-     % If X is greater than the average, include it in the result list
    X > Avg,                                                % Check if X is greater than the average
    greater_than_average(Rest, Avg, Greater).               % Recursively check the rest of the list
greater_than_average([X | Rest], Avg, Greater) :-           % If X is not greater than the average, do not include it
    X =< Avg,                                               % Check if X is less than or equal to the average
    greater_than_average(Rest, Avg, Greater).               % Recursively check the rest of the list

% Query interface
query(Numbers, Avg, Greater) :-                             % Predicate to handle the query - provides a nice wrapper for the user
    average(Numbers, Avg),                                  % Calculate the average of the input list
    greater_than_average(Numbers, Avg, Greater).            % Get the list of numbers greater than the average

% Example query usage:
% ?- query([10, 20, 15, 30, 25], Avg, Greater).
% Output: 
% Avg = 20,
% Greater = [30, 25] ;
% false.
