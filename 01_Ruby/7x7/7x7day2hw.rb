# Day 2 Self-Study (Homework)
# Find:
#   1. How to access files with and without code blocks. What is the benefit of a code block?
#   2. How would you translate a hash to an array? Can you translate arrays to hashes?
#   3. Can you itterate through a hash?
#   4. You can use Ruby arrays as stacks. What other common data structures do arrays support?
# Do:
#   1. Print the contents of an array of sixteen numbers, four numbers at a time, using just each. 
#      Now, do the same with each_slice in Enumerable
#   2. Refactor the Tree class example in day 2 so that the initializer accepts a nested structure of hashes like this:
#      {'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}}, 'uncle' => {'child 3' => {}, 'child 4' => {}}}}
#   3. Write a simple grep that will print the lines of a file having any occurences of a phrase anywhere in that line. 
#      You will need to do a simple regular expression match and read lines from a file

# Find Answers
# 1. How to access files with and without code blocks. What is the benefit of a code block?
#    Without code blocks: You need to explicitly open and close the file using the File.open and File.close methods. 
#    For example:
file = File.open("test.txt", "r")   # open the file in read mode
file.each_line do |line|            # iterate over each line
    puts line                         # print the line
end
file.close # close the file

#    With code blocks: You can pass a code block to the File.open method, 
#    which will automatically close the file after executing the block. 
#    For example:
File.open("test.txt", "r") do |file|    # open the file in read mode and pass it to the block
    file.each_line do |line|            # iterate over each line
        puts line                         # print the line
    end
end                                   # the file is automatically closed at end

# The benefit of using code blocks is that they make the code more concise and readable, 
# and they ensure that the file is always closed properly, even if an exception occurs. 
# Code blocks are also a common idiom in Ruby, and they allow you to use methods like File.foreach, File.read, and File.write 
# that take care of opening and closing the file for you. 
# For example:
File.foreach("test.txt") do |line|  # open the file, iterate over each line, and close the file
    puts line                       # print the line
end

# 2. How would you translate a hash to an array? Can you translate arrays to hashes?
#    In Ruby, you can translate a hash to an array using the to_a method, which returns an array of key-value pairs as subarrays. 
#    For example:
puts
hash = {a: 1, b: 2, c: 3} 
array = hash.to_a          
puts hash
puts array

#    You can also translate an array to a hash using the to_h method, which returns a hash 
#    with the elements of the array as keys and values. 
#    However, the array must be in the form of nested arrays with two elements each, otherwise an exception will be raised. 
#    For example:
puts
array = [["a", 1], ["b", 2], ["c", 3]]  
hash = array.to_h                        
puts array
puts hash

# 3. Can you itterate through a hash?
#    You can iterate through a hash in Ruby using various methods, such as each, each_key, each_value, each_pair, etc. 
#    These methods take a block of code and execute it for each key-value pair in the hash. 
#    For example:
puts 
hash = {a: 1, b: 2, c: 3}
hash.each do |key, value|   # iterate over each key-value pair
    puts "#{key} => #{value}" # print the key and value
end

# 4. You can use Ruby arrays as stacks. What other common data structures do arrays support?
#     Queues:   You can use arrays as queues by adding elements to the end using the push method and removing elements 
#               from the front using the shift method. Queues are useful for implementing first-in, first-out (FIFO) operations.
#     Sets:     You can use arrays as sets by ensuring that they contain only unique elements using the uniq method. 
#               Sets are useful for performing operations such as union, intersection, and difference on collections of items.
#     Matrices: You can use arrays as matrices by creating nested arrays of numbers and using methods from the Matrix class. 
#               Matrices are useful for performing linear algebra operations such as multiplication, inversion, and determinant calculation.
#     Heaps:    You can use arrays as heaps by using the heapify method from the Heap module. 
#               Heaps are useful for implementing priority queues, where the highest or lowest priority element is 
#               always at the top.



# Do Answers
# 1. Print the contents of an array of sixteen numbers, four numbers at a time, using just each. 
#    Now, do the same with each_slice in Enumerable
array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
index = 0
array.each do |num|      # using just each 
    print num, " "         # using print does not create a newline
    index += 1
    puts if index % 4 == 0 # print a new line after every four numbers
end
array.each_slice(4) do |slice|  # using each slice
    puts slice.join(" ")        # print each slice as a space-separated string
end

# 2. Modify the Tree class example in day 2 so that the initializer accepts a nested structure of hashes like this:
#    {'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}}, 'uncle' => {'child 3' => {}, 'child 4' => {}}}}

class Tree 
    attr_accessor :children, :node_name
    def initialize(tree_hash)
        @children = []
        @node_name = tree_hash.keys.first
        tree_hash[@node_name].each do |key, value|
            @children << Tree.new({key => value})
        end
    end
    def visit_all(&block)
        visit &block
        children.each {|c| c.visit_all &block}
    end
    def visit(&block)
        block.call self
    end
end
puts
ruby_tree = Tree.new({'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}}, 'uncle' => {'child 3' => {}, 'child 4' => {}}}})
puts "Visiting a node"
ruby_tree.visit {|node| puts node.node_name}
puts
puts "visiting entire tree"
ruby_tree.visit_all {|node| puts node.node_name}
puts
puts ruby_tree.class

# 3. Write a simple grep that will print the lines of a file having any occurences of a phrase anywhere in that line. 
#    You will need to do a simple regular expression match and read lines from a file. If you want include line numbers
#    version without line numbers
puts
phrase = "Tigers" 
File.open("test.txt", "r") do |file|    # Open the file and read each line
    file.each_line do |line|
        puts line if line =~ /#{phrase}/  # Print the line if it matches the phrase. The =~ method is the pattern match operator
    end
  end

#     version with line numbers
puts
phrase = "Tigers" 
File.open("test.txt", "r") do |file|                           # Open the file and read each line
    file.each_line.with_index do |line, index|
        puts "#{index + 1}: #{line}" if line =~ /#{phrase}/    # Print the line and the line number if it matches the phrase
    end
end

# Additional code to show == vs =~ use
# Using == to compare strings it just looks for an exact match of the strings compared
puts
puts "a" == "a"                         # true
puts "a" == "b"                         # false
puts "a" == "A"                         # false
puts "Tigers" == "Tigers"               # true
puts "There are Tigers" == "Tigers"     # false
puts "Lions" == "Tigers"                # false
# Using =~ to match strings with regexes returns the index of the first match in string, or nil if no match is found.
puts
puts "a" =~ /a/                         # 0
puts "cat" =~ /at/                      # 1
puts "dog" =~ /at/                      # nil
puts "Tigers" =~ /Tigers/               # 0
puts "There are Tigers" =~ /Tigers/     # 10
puts "Lions" =~ /Tigers/                # nil