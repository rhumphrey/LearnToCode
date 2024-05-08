% Lotto Number Generator
% This program prompts the user for a count and range, then generates a list of unique random numbers.
% It uses Prolog's random number generation capabilities to simulate a Lotto draw.

% Import the required library for random number generation
:- use_module(library(random)).                     % Load the random library for generating random numbers

% Entry point for the user to start the Lotto game
start_lotto_game :-
    prompt_count(Count),                                            % Call the predicate to get the number of Lotto numbers to generate
    prompt_range(Range),                                            % Call the predicate to get the range of numbers for the Lotto draw
    generate_unique_random_numbers(Count, Range, LottoNumbers),     % Generate the Lotto numbers
    format('The selected Lotto numbers are: ~w', [LottoNumbers]).   % Print the generated Lotto numbers

% Prompt the user for the number of random numbers to select
prompt_count(Count) :-
    write('Enter the number of random numbers to select: '),        % Ask the user for the count
    read(Count),                                                    % Read the user's input
    integer(Count),                                                 % Check if the input is an integer
    Count > 0.                                                      % Ensure the input is greater than 0

% Prompt the user for the maximum number from which to select
prompt_range(Range) :-
    write('Enter the maximum number from which to select: '),       % Ask the user for the range
    read(Range),                                                    % Read the user's input
    integer(Range),                                                 % Check if the input is an integer
    Range > 0.                                                      % Ensure the input is greater than 0

% Generate a list of unique random numbers within a specified range
generate_unique_random_numbers(Count, Range, NumbersList) :-
    findall(Number, between(1, Range, Number), AllNumbers),         % Create a list of all possible numbers
    random_permutation(AllNumbers, PermutedNumbers),                % Shuffle the list of all numbers
    take(Count, PermutedNumbers, NumbersList).                      % Take the first 'Count' numbers from the shuffled list

% Take the first N elements from a list
take(NumberOfElements, List, TakenElements) :- 
    take_helper(NumberOfElements, List, [], TakenElements).         % Call the helper function to take elements

% Helper predicate for take/3 to accumulate the result
take_helper(0, _, Accumulator, Accumulator) :- !.                   % If no more elements are needed, return the accumulator
take_helper(NumberOfElements, [Head|Tail], Accumulator, TakenElements) :-
    NumberOfElements > 0,                                           % Check if more elements are needed
    UpdatedNumberOfElements is NumberOfElements - 1,                % Decrease the number of elements needed by one
    take_helper(UpdatedNumberOfElements, 
        Tail, [Head|Accumulator], TakenElements).                   % Recursively call the helper with the updated number of elements
