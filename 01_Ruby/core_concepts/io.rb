# writing and reading text files
# Open the file output.txt in write mode
file = File.open("output.txt", "w")
# Write some data to the file
file.write("Hello, World!\n")
# Close the file
file.close

# Read the file users.txt into a string
content = File.read("test.txt")
# Print the content
puts content

# Open the file users.txt and read it line by line
File.open("test.txt") do |file|
    file.each_line do |line|
      # Print each line
      puts line
    end
  end

# Iterate over each line of users.txt and print it
IO.foreach("test.txt") do |line|
    puts line
end

# writing and reading csv
require 'csv'
# open a new file in write mode
CSV.open("my_file.csv", "w") do |csv|
  # append a row of headers
  csv << ["name", "age", "city"]
  # append some data rows
  csv << ["Belinda", 25, "Halifax"]
  csv << ["Zebedee", 32, "Toronto"]
  csv << ["Ezra", 28, "Vancouver"]
end

# iterate over each row of the file
CSV.foreach("my_file.csv") do |row|
  # row is an array of fields
  puts row.inspect
end

# iterate over each row of the file with headers
CSV.foreach("my_file.csv", headers: true) do |row|
  # row is a CSV::Row object
  puts row["name"] # access by header
  puts row[0] # access by index
end

# user i/o
# Prompt the user for their name
print "What is your name? "
# Read the user input
name = gets.chomp
# Print a greeting message
puts "Hello, #{name}!"