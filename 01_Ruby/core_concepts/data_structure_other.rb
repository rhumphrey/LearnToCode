# String: A string is a sequence of characters that can be manipulated with various methods. 
# You can use strings to store and process text, such as names, messages, commands, etc.

# Create a string
name = "Ruby"

# Access a character by index
puts name[0] # => "R"

# Concatenate two strings
puts name + " is awesome" # => "Ruby is awesome"

# Reverse a string
puts name.reverse # => "ybuR"

# Range: A range is a collection of values that span from a start point to an end point. 
# You can use ranges to represent intervals, sequences, or conditions.

# Create a range of numbers
numbers = 1..10

# Check if a value is in the range
puts numbers.include?(5) # => true

# Convert a range to an array
print numbers.to_a # => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
puts
print numbers
puts
array_of_numbers = numbers.to_a
print array_of_numbers
puts

# Use a range as a condition
age = 56
case age
when 0..17
  puts "You are a minor"
when 18..64
  puts "You are an adult"
else
  puts "You are a senior"
end
puts

# Queue: A queue is a collection of elements that follow the first-in, first-out (FIFO) principle. 
# You can use queues to store and retrieve data in the order they were added. 

# Create a queue
queue = Queue.new

# Add elements to the queue
queue << "Ada"
queue << "Guido"
queue << "Matz"
# Remove elements from the queue
until queue.empty? 
  puts queue.pop
end
# Removing elements from the queue with pop they come off as below
# queue.pop # => "Ada"
# queue.pop # => "Guido"
# queue.pop # => "Matz"
puts
# Its interesting at this point to re-visit arrays and see what happens under the same order of element adding and removing
# Create an array
array = []
# Add elements to the queue
array << "Ada"
array << "Guido"
array << "Matz"
# Remove elements from the queue
until array.empty? 
  puts array.pop
end
# Removing elements from the array with pop they come off as below
# array.pop # => "Matz"
# array.pop # => "Guido"
# array.pop # => "Ada"
# what we have here is last-in, first-out (LIFO) aka first in, last-out (FILO) aka a stack
puts
# watch what happens when we change our removal method by changing pop for shift
array_q = []
# Add elements to the queue
array_q << "Ada"
array_q << "Guido"
array_q << "Matz"
# Remove elements from the queue
until array_q.empty? 
  puts array_q.shift
end
# Removing elements from the array with shift they come off as below
# queue.pop # => "Ada"
# queue.pop # => "Guido"
# queue.pop # => "Matz"
# what we have here is first-in, first-out (FIFO) aka last-in, last-out (LILO) aka a queue
# what might be implications of implementing it this way as oppesed to using a ruby gem like 'deque' or 'linked-list'

# remember its always interesting to use class method on an object in Ruby
puts queue.class 
puts array.class
puts array_q.class

 