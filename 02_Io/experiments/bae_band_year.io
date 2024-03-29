// WIP - Playing with data from a csv:)

baeList

// Read in the bae.csv file
file := File with("albums/bae.csv") 
file openForReading
fileContent := file readToEnd
file close

// Split the content into lines
lines := fileContent split("\n")
// Using 'foreach' method to iterate over lines
lines foreach(line, 
    // Split each line by comma to get individual values
    values := line split(",")
    // Using 'foreach' method to iterate over values
    values foreach(value, writeln(value))
)