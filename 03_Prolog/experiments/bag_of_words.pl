% WIP - Still needs work on the threshold/2 and top_words/2 predicates and related to determining top 20% of word count 

% Predicate to read file content and convert it to a list of words, excluding punctuation and specific words
read_file_to_words(FileName, Words) :-
    read_file_to_string(FileName, Content, []),                                             % Reads the file content into Content
    replace_punctuation(Content, NoPunctContent),                                           % Removes punctuation from Content
    split_string(NoPunctContent, " ", "\n\t\r", StrWords),                                  % Splits NoPunctContent into words
    maplist(string_lower, StrWords, LowerWords),                                            % Converts words to lowercase
    exclude(is_specific_word, LowerWords, FilteredWords),                                   % Removes specific words
    maplist(atom_string, Words, FilteredWords).                                             % Converts filtered strings to atoms

% Helper predicate to remove punctuation
replace_punctuation('', '').                                                                % Base case for an empty string
replace_punctuation(Content, NoPunctContent) :-
    atom_codes(Content, Codes),                                                             % Converts Content to character codes
    exclude(is_punctuation, Codes, NoPunctCodes),                                           % Removes punctuation codes
    atom_codes(NoPunctContent, NoPunctCodes).                                               % Converts character codes back to string

% Check if a code is a punctuation mark using ASCII codes
is_punctuation(Code) :-
    PunctuationCodes = [46, 44, 59, 58, 33, 63, 34, 39, 45, 40, 41, 91, 93, 123, 125],          % ASCII codes for punctuation marks
    memberchk(Code, PunctuationCodes).                                                      % Succeeds if Code is an ASCII code for a punctuation mark

% Check if a word is one of the specific words to be removed
is_specific_word(Word) :-
    SpecificWords = ["a", "we", "the", "of", "to", "and", "our", "or", "for", "by"],        % List of specific words to remove
    memberchk(Word, SpecificWords).                                                         % Succeeds if Word is a specific word to be removed

% Count the occurrences of each word
count_words([], []).                                                                        % Base case for an empty list
count_words([H|T], [H-Count|Counts]) :-
    aggregate_all(count, member(H, T), Count1),                                             % Counts occurrences of H in T
    Count is Count1 + 1,                                                                    % Adjusts count for the current word
    exclude(==(H), T, Rest),                                                                % Excludes current word from the rest of the list
    count_words(Rest, Counts).                                                              % Recursively counts occurrences of words

% Sort the list of words based on their frequency
sort_words(Words, Sorted) :-
    count_words(Words, Counts),                                                             % Counts occurrences of each word
    sort(2, @>=, Counts, Sorted).                                                           % Sorts words by frequency

% Calculate the threshold for the top 20% most used words
threshold([], 0).                                                                           % Base case for an empty list
threshold(Words, Threshold) :-
    length(Words, Total),                                                                   % Calculates total number of words
    Top20Percent is Total // 5,                                                             % Determines top 20% of words
    Top20Percent > 0,                                                                       % Ensures there is at least one word
    length(TopWords, Top20Percent),                                                         % Gets the top 20% most frequent words
    append(TopWords, _, Words),                                                             % Appends top words to the rest
    last(TopWords, _-Threshold).                                                            % Finds the frequency threshold

% Find the words that meet the threshold
top_words(Words, TopWords) :-
    sort_words(Words, Sorted),                                                              % Sorts words by frequency
    threshold(Sorted, Threshold),                                                           % Calculates frequency threshold
    include([_-Count]>>(Count >= Threshold), Sorted, TopWords).                             % Filters words by threshold

% Main predicate to load the file and find the top words
analyze_top_words(FileName) :-
    read_file_to_words(FileName, Words),                                                    % Reads file and creates a list of words
    top_words(Words, TopWords),                                                             % Finds the top words
    writeln('Top 20% most used words:'),                                                    % Outputs header
    maplist([Word-Count]>>format('~w: ~d times\n', [Word, Count]), TopWords).               % Prints each word and count


% Example query to run the analysis
% ?- analyze_top_words('document.txt').


% Additional debug predicates all below this comment
% Test case to check if punctuation is removed
test_remove_punctuation :-
    TestString = 'Hello, world! This is a test.',
    ExpectedResult = 'Hello world This is a test',
    replace_punctuation(TestString, NoPunctString),
    NoPunctString == ExpectedResult.
/* % Alternate replace_punctuation/2 predicate for testing character removal
replace_punctuation(Content, NoPunctContent) :-
    atom_codes(Content, Codes),
    format('Original Codes: ~w~n', [Codes]),
    exclude(is_punctuation, Codes, NoPunctCodes),
    format('No Punctuation Codes: ~w~n', [NoPunctCodes]),
    atom_codes(NoPunctContent, NoPunctCodes).
*/

% Predicate to count the total number of unique words
count_unique_words(Words, UniqueCount) :-
    sort(Words, UniqueWords),                                                               % Sorts the list of Words, removing duplicates, resulting in UniqueWords.
    length(UniqueWords, UniqueCount).                                                       % Calculates the length of the UniqueWords list, which is the count of unique words.

% Predicate to count the total number of words (including duplicates)
count_all_words(Words, TotalCount) :-
    length(Words, TotalCount).                                                              % Calculates the length of the Words list, which is the total count of all words.

% Predicate to calculate the expected number of top words (20%)
expected_top_words_count(FileName, ExpectedCount, UniqueCount, TotalCount) :-
    read_file_to_words(FileName, Words),                                                    % Reads the contents of a file, and stores the words in the list Words.
    count_unique_words(Words, UniqueCount),                                                 % Uses the above predicate to count unique words in Words.
    count_all_words(Words, TotalCount),                                                     % Uses the above predicate to count all words in Words.
    ExpectedCount is UniqueCount // 5,                                                      % Divides the UniqueCount by 5 to find 20% of the words.
    format('Total number of unique words: ~d\n', [UniqueCount]),                            % Outputs the count of unique words.
    format('Expected number of top words (20%): ~d\n', [ExpectedCount]),                    % Outputs the expected count of top words.
    format('Total number of all words: ~d\n', [TotalCount]).                                % Outputs the total count of all words.

% Example query to run the debug/testing predicate expected_top_words_count/4
% ?- expected_top_words_count('document.txt', ExpectedCount, UniqueCount, TotalCount).