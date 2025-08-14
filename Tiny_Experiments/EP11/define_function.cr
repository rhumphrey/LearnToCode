def add_numbers(a : Int32, b : Int32) : Int32
  a + b # The last expression is implicitly returned
end

result = add_numbers(10, 25)
puts "The sum of 10 and 25 is: #{result}"