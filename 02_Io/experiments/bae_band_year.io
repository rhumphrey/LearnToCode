// WIP - Playing with data from a csv:)

// heading method
heading := method(title, do(
    (title size) repeat("-" print)
    "" println
    title println
    (title size) repeat("-" print)
    "" println
    )
)

includeColumnsTo := 9                       // Set the number of columns to include in the output
bandToSearch := "Bloc Party"                        // Define the band name to search for in the CSV
heading(bandToSearch)
previousRank := "0"
currentRank := "0"


// Read in the 'bae.csv' file from the 'albums' directory
file := File with("albums/bae.csv") 
file openForReading                         // Open the file for reading
fileContent := file readToEnd               // Read the entire file content into a string
file close                                  // Close the file to free resources

// Split the file content into an array of lines
lines := fileContent split("\n")

// Iterate over each line in the 'lines' array
lines foreach(line, 
    // Split the line into an array of values using the comma as a delimiter
    values := line split(",")
    // Check if the second value (band name) matches 'bandToSearch' or is "Band"
    if((values at(1)) == bandToSearch or values at(1) == "Band", 
        colCount := 1                       // Initialize a counter for the columns
        // Iterate over each value in the 'values' array
        values foreach(value, 
            // If the column count exceeds the set limit, exit the loop
            if(colCount > includeColumnsTo, break)
            write(value .. ", ")            // Write the value followed by a comma to the console
            colCount = colCount + 1         // Increment the column counter
            currentRank = value  
        )
         // Write a newline character to separate each band's data
        if(values at(1) == "Band", 
            writeln("RankDiff")
            currentRank = "0", 
            writeln(previousRank asNumber - currentRank asNumber)
        ) 
        previousRank = currentRank
        
    )
)
writeln("")