# frozen_string_literal: true

# This problem was asked by Amazon
# Given a pivot x, and a list lst, partition the list into three parts.
# •	The first part contains all elements in lst that are less than x
# •	The second part contains all elements in lst that are equal to x
# •	The third part contains all elements in lst that are larger than x
# Ordering within a part can be arbitrary.
# For example, given x = 10 and lst = [9, 12, 3, 5, 14, 10, 10], one partition may be [9, 3, 5, 10, 10, 12, 14].

# this method creates a hash of arrays
# Define a method that takes a list and a pivot value as arguments
def partition_hash(lst, pivot)
  # Use the group_by method to split the list into three parts
  lst.group_by do |n|
    if n < pivot # Compare each element to the pivot value
      :less                       # Return :less for elements less than pivot
    elsif n == pivot
      :equal                      # Return :equal for elements equal to pivot
    else
      :greater                    # Return :more for elements more than pivot
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

# A variation of the above to using a hash and group_by to return an array
# The || [] ensures that if there are no elements in one of the categories, it returns an empty array instead of nil.
def partition_array(lst, pivot)
  groups = lst.group_by do |elem|
    if elem < pivot
      :less
    elsif elem == pivot
      :equal
    else
      :greater
    end
  end

  [groups[:less] || [], groups[:equal] || [], groups[:greater] || []]
end

# Create a list of 10 numbers
numbers = [9, 12, 3, 5, 14, 10, 10]

# Call the partition method with the list and a pivot value of 5
result = partition_array(numbers, 5)

# Print the result
print result
puts


# a version just using built in .partition method
def partition(lst, pivot)
  # Use the built-in partition method
  lst.partition { |n| n < pivot }
end

# Create a list of 10 numbers
numbers = [9, 12, 3, 5, 14, 10, 10]

# Call the partition method with the list and a pivot value of 5
result = partition(numbers, 5)

# Print the result
print result
puts
