require 'csv'

# Data comes from Best Ever Albums site - Uncut Top 100 80s Albums
# Read the csv into a CSV::Table
# Uncut100_80s.csv has the follwing headings Rank,Title,Band,Country,Year,Compilation,Live,YearRank,DecadeRank,OverallRank,RankScore,AvgRating,NumRatings,AlbumID,Notes
table = CSV.read("Uncut100_80s.csv", headers: true)

# Commented out code - prints the full list of album titles and bands in the original order in csv - on Rank
# Loop over each row array
# table.each do |row|
#     puts "#{row["Title"]} - #{row["Band"]}"                     # Print the title and band information
# end

groups = table.group_by {|row| row["Year"]}                     # Group the rows by year
subtotals = groups.map {|year, rows| [year, rows.count]}        # Count the entries for each year
subtotals.sort_by! {|year, count| year.to_i}                    # Sorting in place

# Print the subtotals as a bar of #
puts
puts "Data from Uncut Top 100 80s Albums"
puts "Number of albums per year for 1980s"
subtotals.each do |year, count|
  bar = "#" * count                                             # Create a # bar of the length count
  puts "#{year}: #{bar} [#{count}]"
end

# Print a list of the albums for each year sorted by AvgRating
groups.sort_by {|year, rows| year.to_i}.each do |year, rows|
    puts "\nAlbums in #{year}:"
    rows.sort_by! {|row| row["AvgRating"].to_f}.reverse!        # Sort the rows by AvgRating in descending order
    rows.each do |row|
      avg_rating = row["AvgRating"].to_f.round(2)               # AvgRating only needs 2 decimal places 
      puts "#{row["Title"]} - #{row["Band"]} [#{avg_rating}]"
    end
end

# get the average rating of all albums to 2 decimal places
avg_rating = (table["AvgRating"].map(&:to_f).sum / table.size).round(2) 
puts
puts "Average ranking of all these albums on Best Ever Albums: #{avg_rating}"