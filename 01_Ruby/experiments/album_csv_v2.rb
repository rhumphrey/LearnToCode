# 1st refactoring from album_csv_v1.rb

require 'csv'

# Data comes from Best Ever Albums site - Uncut Top 100 80s Albums
# Read the csv into a CSV::Table
# Uncut100_80s.csv has the follwing headings Rank,Title,Band,Country,Year,Compilation,Live,YearRank,DecadeRank,OverallRank,RankScore,AvgRating,NumRatings,AlbumID,Notes
table = CSV.read("Uncut100_80s.csv", headers: true)

# Print the subtotals as a bar of #
puts
puts "Data from Uncut Top 100 80s Albums"
puts "Number of albums per year for 1980s"
subtotals = table.by_col["Year"].group_by(&:itself).transform_values(&:count)
subtotals.sort.each do |year, count|
  bar = "#".ljust(count, "#")                                                   # Create a # bar of the length count
  puts "#{year}: #{bar} [#{count}]"
end

# Print a list of the albums for each year sorted by AvgRating
table.group_by {|row| row["Year"]}.sort.each do |year, rows|
    puts "\nAlbums in #{year}:"
    rows.sort_by! {|row| [-row["AvgRating"].to_f, row["Title"], row["Band"]]}   # Sort the rows by AvgRating in descending order
    rows.each do |row|
      avg_rating = "%.2f" % row["AvgRating"].to_f                               # AvgRating only needs 2 decimal places 
      puts "#{row["Title"]} - #{row["Band"]} [#{avg_rating}]"
    end
end

# get the average rating of all albums to 2 decimal places
avg_rating = "%.2f" % (table.by_col["AvgRating"].sum(&:to_f) / table.size)
puts
puts "Average ranking of all these albums on Best Ever Albums: #{avg_rating}"