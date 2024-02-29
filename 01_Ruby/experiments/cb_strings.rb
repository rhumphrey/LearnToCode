# Bulding a string from a hash data structure
books = { fiction: 12, nonfiction: 8, poetry: 4 }
output = ""
# Iterate over each key-value pair in the hash
books.each_pair do |genre, count|
  output << "#{genre.capitalize} books: #{count}\n"             # Append the genre and count to the output string
end
puts output

