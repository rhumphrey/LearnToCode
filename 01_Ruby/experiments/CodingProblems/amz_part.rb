# This problem was asked by Amazon.
# Given a pivot x, and a list lst, partition the list into three parts.
# •	The first part contains all elements in lst that are less than x
# •	The second part contains all elements in lst that are equal to x
# •	The third part contains all elements in lst that are larger than x
# Ordering within a part can be arbitrary.
# For example, given x = 10 and lst = [9, 12, 3, 5, 14, 10, 10], one partition may be [9, 3, 5, 10, 10, 12, 14].

# this method creates a hash of arrays
# Define a method that takes a list and a pivot value as arguments
def partition_hash(lst, x)
  # Use the group_by method to split the list into three parts
  lst.group_by do |n|
    if n < x # Compare each element to the pivot value
      :less                       # Return :less for elements less than x
    elsif n == x
      :equal                      # Return :equal for elements equal to x
    else
      :greater                    # Return :more for elements more than x
    end
  end
end
  
# Create a list of 10 numbers
numbers = [9, 12, 3, 5, 14, 10, 10]

# Call the partition method with the list and a pivot value of 5
result = partition_hash(numbers, 5)

# Print the result
print result
puts

# this methods creates an array of arrays
def partition_array(lst, x)
  # Initialize three empty arrays to store the elements in each part
  less = []
  equal = []
  greater = []
  
  # Loop through each element in the list
  lst.each do |elem|
    # Compare the element with the pivot and append it to the corresponding array
    if elem < x
      less << elem
    elsif elem == x
      equal << elem
    else
      greater << elem
    end
  end
  # Return the concatenation of the three arrays
  return [less] + [equal] + [greater]
end

  # Create a list of 10 numbers
numbers = [9, 12, 3, 5, 14, 10, 10]

# Call the partition method with the list and a pivot value of 5
result = partition_array(numbers, 5)

# Print the result
print result
puts


# a version just using built in .partition method
def partition(lst, x)
    # Use the built-in partition method
    lst.partition { |n| n < x }
end
  
  # Create a list of 10 numbers
  numbers = [9, 12, 3, 5, 14, 10, 10]
  
  # Call the partition method with the list and a pivot value of 5
  result = partition(numbers, 5)
  
  # Print the result
  print result
  puts