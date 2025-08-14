i = 0
puts "While loop:"
while i < 3
  puts "  Iteration #{i}"
  i += 1
end

puts "\nEach loop over a range:"
(1..3).each do |num|
  puts "  Number: #{num}"
end