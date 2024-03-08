require 'csv'

# Read only the relevant columns from the csv file
table = CSV.read('albums/bae.csv', headers: true).map { |row| row.to_h }
df = table.select { |row| ["Band", "Title", "Year", "YearRank", "DecadeRank", "OverallRank", "RankScore", "NumRatings"].all? { |col| row.key?(col) } }

# Remove rows where YearRank is NaN
df.reject! { |row| row['YearRank'].nil? }

# Convert values to integers
df.each do |row|
  ['Year', 'YearRank', 'DecadeRank', 'OverallRank', 'RankScore', 'NumRatings'].each do |col|
    row[col] = row[col].to_i
  end
end

# Sort on Rank
df.sort_by! { |row| row['OverallRank'] }

# Prompt for artist input
puts "Enter a favourite musical artist name: "
artist = gets.chomp

# Filter by artist and select only OverallRank, Title, and Year columns
select_artist = df.select { |row| row["Band"] == artist }.map { |row| row.values_at("OverallRank", "Title", "Year") }

filename = "#{artist} - Same Year.txt"
File.open(filename, 'a:UTF-8') do |f|
  # Convert to string and remove index
  f.puts "\nFor Artist: #{artist.upcase}"
  f.puts "----------------------------------------------------------------------------"
  f.puts "The Album Rankings and Top 10 Other Albums That Year From BestEverAlbums.com"
  f.puts "----------------------------------------------------------------------------"
  select_artist.each do |row|
    title, year = row[1], row[2]
    f.puts "#{title} - #{year}"
    select_year = df.select { |r| r['Year'] == year }.first(10)
    select_year.each do |r|
      f.puts " #{r['Band']} - #{r['Title']}"
    end
    f.puts
  end
end
