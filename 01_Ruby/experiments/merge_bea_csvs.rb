

# Require the csv library
require 'csv'

# Initialize an empty array to store the data
data = []

# Loop through all the csv files in the bae subfolder
Dir.glob("bae/*.csv") do |file|
  # Read the csv file and skip the header row
  CSV.foreach(file, headers: true) do |row|
    # Check if the OverallRank field is not empty
    unless row["OverallRank"].nil? or row["OverallRank"].empty?
      # Add the row to the data array
      data << row
    end
  end
end

# Write the data to a single csv file with the header
CSV.open("bae_combined.csv", "w") do |csv|
  # Write the header row
  csv << data.first.headers
  # Write the data rows
  data.each do |row|
    csv << row
  end
end
