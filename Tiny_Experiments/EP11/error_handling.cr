input_str = "not_a_number"

begin
  # Attempt to convert a string to an integer
  num_val = input_str.to_i
  puts "Successfully converted '#{input_str}' to #{num_val}"
rescue ex : ArgumentError
  # Catch a specific ArgumentError if conversion fails
  puts "Caught an error: Could not convert '#{input_str}' to a number. Message: #{ex.message}"
rescue ex
  # Catch any other unexpected error
  puts "Caught an unexpected error: #{ex.message}"
end