# Array: An array is a collection of ordered, indexed elements that can be of any type. 
# You can use arrays to store lists of items, iterate over them, access them by index, and perform various operations on them
# Array examples
fruits = ["apple", "banana", "orange", "grape"] # Create an array of fruits
puts fruits
print fruits
puts

puts fruits[0]                                  # Access the first element

fruits << "pear"                                # Add a new element to the end using <<

print fruits
puts

fruits.push("lemon")                            # Add a new element to the end using push

print fruits
puts

fruits.pop                                      # Remove the last element

print fruits
puts

fruits.sort                                     # Sort the array without changing the array

print fruits
puts

print fruits.sort                               

puts

sorted_fruits = fruits.sort                     # create a sorted copy

print sorted_fruits
puts
print fruits
# puts fruits.methods.sort