# Predicate methods on numbers
x = 5
puts x.even?                # => false
puts x.odd?                 # => true
puts x.between?(1, 10)      # => true

# Predicate methods on strings
name = "Ruby"
puts name.start_with?("R")  # => true
puts name.include?("y")     # => true

# Predicate methods on arrays
arr = [1, 2, 3]
puts arr.empty?             # => false
puts arr.include?(2)        # => true