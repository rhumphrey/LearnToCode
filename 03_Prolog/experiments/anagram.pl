anagram(Word, Candidates, Anagrams) :-   
    downcase_atom(Word, LowerWord),                     % Convert the Word to lowercase to make the comparison case-insensitive.
    atom_chars(LowerWord, WordChars),                   % Convert the lowercase Word into a list of characters (WordChars).
    msort(WordChars, SortedWordChars),                  % Sort the characters of the Word, keeping duplicates (SortedWordChars).
    findall(Candidate, (                                % Find all Candidates that are anagrams of the Word.
        member(Candidate, Candidates),                  % Check if Candidate is in the list of Candidates.
        downcase_atom(Candidate, LowerCandidate),       % Convert the Candidate to lowercase.
        LowerWord \= LowerCandidate,                    % Ensure the lowercase Candidate is not the same as the lowercase Word.
        atom_chars(LowerCandidate, CandidateChars),     % Convert the lowercase Candidate into a list of characters (CandidateChars).
        msort(CandidateChars, SortedCandidateChars),    % Sort the characters of the Candidate, keeping duplicates (SortedCandidateChars).
        SortedWordChars == SortedCandidateChars         % Check if the sorted character lists of the Word and Candidate are equal.
    ), Anagrams).                                       % Return the list of Anagrams that match the Word.