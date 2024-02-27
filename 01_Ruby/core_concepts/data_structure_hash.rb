# Hash: A hash is a collection of key-value pairs that can be of any type. 
# You can use hashes to store mappings between keys and values, such as dictionaries, phone books, or configurations.

# Create a hash of countries and their capitals
countries = {"Canada" => "Ottawa", "France" => "Paris", "Japan" => "Tokyo"}

# Access the value by key
puts countries["France"] # => "Paris"

# Add a new key-value pair
countries["Brazil"] = "Brasilia"

# Delete a key-value pair
countries.delete("Japan") # => "Tokyo"

# Iterate over the hash
countries.each do |key, value|
  puts "#{key} has #{value} as its capital"
end