#### WIP ####
# Import the necessary library if available
# CSV parsing has to be implemented or a library needs to be used

# Read only the relevant columns from the csv file
data := list()
File with("albums/bae.csv") openForReading do(f,
    # Assuming a CSV reader is available or implemented
    reader := CSVReader clone setFile(f)
    reader readLines map(row, 
        if(row at("YearRank") != "", data append(row))
    )
)

# Convert values to integers
data foreach(row, 
    ["Year", "YearRank", "DecadeRank", "OverallRank", "RankScore", "NumRatings"] foreach(key,
        row atPut(key, (row at(key) asNumber))
    )
)

# Sort on Rank
data sortInPlace(block(row, row at("OverallRank")))

# Prompt for artist input
artist := File standardInput readLine("Enter a favourite musical artist name: ")

# Filter by artist and select only OverallRank, Title, and Year columns
select_artist := data select(row, row at("Band") == artist)

filename := artist .. " - Same Year" .. ".txt"
File clone open(filename, "w") do(f,
    f write("\nFor Artist: " .. artist asUppercase .. "\n")
    f write("----------------------------------------------------------------------------\n")
    f write("The Album Rankings and Top 10 Other Albums That Year From BestEverAlbums.com\n")
    f write("----------------------------------------------------------------------------\n")
    select_artist foreach(row,
        f write(row at("Title") .. " - " .. row at("Year") .. "\n")
        album_year := row at("Year")
        select_year := data select(row, row at("Year") == album_year)
        select_year sortInPlace(block(row, row at("OverallRank")))
        select_year first(10) foreach(row,
            f write(" " .. row at("Band") .. " - " .. row at("Title") .. "\n")
        )
        f write("\n")
    )
)
