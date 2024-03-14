# frozen_string_literal: true

# This problem was asked by Amazon.
# Implement a stack API using only a heap. A stack implements the following methods:
# •	push(item), which adds an element to the stack
# •	pop(), which removes and returns the most recently added element (or throws an error if nothing is on the stack)
# Recall that a heap has the following operations:
# •	push(item), which adds a new key to the heap
# •	pop(), which removes and returns the max value of the heap

require 'algorithms' # Make sure to include the 'algorithms' gem in your Gemfile - "gem install algorithms"

# The Stack class provides a simple stack implementation using a max-heap.
# This data structure follows the Last In, First Out (LIFO) principle,
# where the most recently added element is the first to be removed.
#
# Each element is augmented with a monotonically increasing index to ensure
# that the max-heap behaves like a stack. The `algorithms` gem is used to
# provide the max-heap functionality.
#
# Example:
#   stack = Stack.new
#   stack.push(3)
#   stack.push(5)
#   stack.push(7)
#   puts stack.pop # => 7
#   puts stack.pop # => 5
#   puts stack.pop # => 3
#
# Methods:
#   push(item) - Adds an item to the stack.
#   pop() - Removes and returns the most recently added item from the stack.
#
# Note: Attempting to pop from an empty stack will raise an error with the message "Stack is empty".
class Stack
  def initialize
    @heap = Containers::MaxHeap.new
    @index = 0
  end

  def push(item)
    @heap.push([@index, item])
    @index += 1
  end

  def pop
    raise 'Stack is empty' if @heap.empty?

    @heap.pop[1] # Return only the item, not the timestamp
  end
end

# Example usage:
stack = Stack.new
stack.push(3)
stack.push(5)
stack.push(7)
puts stack.pop # => 7
puts stack.pop # => 5
puts stack.pop # => 3