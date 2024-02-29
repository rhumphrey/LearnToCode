# Define a method that takes an array and a value as arguments
def linear_search(array, value)
  # Loop through the array with the index
  array.each_with_index do |element, index|
    # Return the index if the element matches the value
    return index if element == value
  end
  # Return nil if no match is found
  nil
end

numbers = [1,6,3,7,3,9,5,7,3,5]
result = linear_search(numbers, 7)
if result.nil?
    puts "The result is nil"
  else
    puts result
end