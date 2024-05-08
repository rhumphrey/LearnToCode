% Define the complement of each nucleotide
complement('G', 'C').                               % Complement of Guanine is Cytosine
complement('C', 'G').                               % Complement of Cytosine is Guanine
complement('T', 'A').                               % Complement of Thymine is Adenine
complement('A', 'U').                               % Complement of Adenine in RNA is Uracil

% Helper predicate to transcribe each nucleotide in the string
transcribe_nucleotides([], []).                     % Base case: an empty list transcribes to an empty list
transcribe_nucleotides([Head|Tail], [TranscribedHead|TranscribedTail]) :-
    complement(Head, TranscribedHead),              % Find the complement of the head nucleotide
    transcribe_nucleotides(Tail, TranscribedTail).  % Recursively transcribe the rest of the list

% Main predicate to transcribe a DNA string to an RNA string
rna_transcription(DnaStr, RnaStr) :-
    string_chars(DnaStr, DnaList),                  % Convert the DNA string to a list of characters
    transcribe_nucleotides(DnaList, RnaList),       % Transcribe the nucleotides to their RNA complements
    string_chars(RnaStr, RnaList).                  % Convert the transcribed list back to a string
