// Read text from a file (assuming 'filename.txt' contains your text)
file := File with("document.txt") 
file openForReading
text := file readToEnd
file close

// Preprocess text: remove punctuation and split into words
charactersToRemove := list(",", ".", ":", "(", ")", "-")
cleanText := text
charactersToRemove foreach(char, cleanText = cleanText asList select(x, x != char) join(""))
wordList := cleanText split

// Create a Set to store unique words (vocabulary)
vocabulary := List clone

// Add words to the vocabulary
wordList foreach(word,
    if((vocabulary contains(word)) == false, vocabulary append(word))
)

// Initialize a Bag of Words vector with zeros
bowVector := List clone 
for(n, 1, (vocabulary size), bowVector append(0))

# Count word occurrences and update the vector
wordList foreach(word,
    index := vocabulary indexOf(word)
    if(index > -1,
        bowVector atPut(index, 1 + bowVector at(index))        
    )
)

// Now 'bowVector' contains your Bag of Words representation
// Each element corresponds to the count of a word in the vocabulary

// Print the word and count 
for(n, 1, (vocabulary size) - 1, writeln(vocabulary at(n) .. " = " .. bowVector at(n)))