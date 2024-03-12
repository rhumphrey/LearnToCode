# Day 2 experiments


# Functions
def tell_the_truth
    true
end

puts tell_the_truth

# Arrays
animals = ['lions', 'tigers', 'bears']
puts
puts animals
puts animals[0]
puts animals[2]
puts animals[10]
puts animals[-1]
puts animals[-2]
puts animals[0..1]
puts animals.class
puts animals[0].class
puts animals[10].class
puts animals[0..1].class
puts [0..1].class
puts (0..1).class
puts {0..1}.class

# notice if I try and run the following I will get an error (I'm using begin and rescue to trap the error)
# a[0] = 'lion'

begin
    a[0] = 'lion'
rescue NameError => e
    puts e.exception
end

# More arrays
puts [1].class
puts [1].methods.include?(:[])

a = []  # I've created an empty array - Is I add to it notice it does not have to be homogeneous
a[0] = 'zero'
a[1] = 1
a[2] = ['two', 'things']
puts a
puts a.class
for i in 0..2
    puts a[i].class
end

a = [[1, 2, 3], [10, 20, 30], [100, 200, 300]] # multidimentional arrays are arrays of arrays
puts a
for i in 0..2
    for j in 0..2
        puts a[i,j]
    end
end

a = [1]
puts a
a.push(3)
puts a
a = [4]
puts a
a.push(2)
puts a
a.pop
puts a
a.pop
puts a # notice nothing is displayed on the screen not even a newline
puts a.class

# Hashes
numbers = {1 => 'one', 2 => 'two'}
puts
puts numbers
puts numbers[1]
puts numbers[2]
puts numbers.class
puts numbers[1].class

stuff = {:array => [1, 2, 3], :string => 'Hi, mom!'}
puts stuff
puts stuff[:string]
puts stuff[:array]
puts stuff.class
puts stuff[:string].class
puts stuff[:array].class
puts 'string'.object_id
puts 'string'.object_id
puts ':string'.object_id
puts ':string'.object_id
puts 'array'.object_id
puts 'array'.object_id
puts ':array'.object_id
puts ':array'.object_id

a = 'Hello' # a quick side-quest / experiment with object_id
b = 'Hello'
c = a
puts a.object_id
puts b.object_id
puts c.object_id

def tell_the_truth(options={}) # named parameters can be simulated with a hash
    if options[:profession] == :lawyer
        'it could be belived that this is almost certainly not false.'
    else
        true
    end
end

puts tell_the_truth
puts tell_the_truth( :profession => :lawyer)

# Code blocks and yield (code blocks are functions without names use {} for 1-line and do/end for multi)
puts
3.times {puts 'hiya there, kiddo'} 

animals = ['lions and', 'tigers and', 'bears', 'oh my']
animals.each {|a| puts a}

class Integer # Fixnum as used in the 7 in 7 book is depricated
    def my_times
        i = self
        while i > 0
            i = i - 1
            yield
        end
    end
end
3.my_times {puts 'mangy moose'}

def call_block(&block) # you can use blocks to pass around executable code
    block.call
end
def pass_block(&block)
    call_block(&block)
end
pass_block {puts 'Hello, block'}
# Note: in ruby blocks cam be used to delay execution, conditinally execute something, enforce policy, etc

# Running Ruby from a file - at the command line :> ruby filename

# Defining classes
puts
puts 4.class
puts 4.class.superclass
puts 4.class.superclass.superclass
puts 4.class.superclass.superclass.superclass
puts 4.class.superclass.superclass.superclass.superclass

puts 4.class.class
puts 4.class.class.superclass
puts 4.class.class.superclass.superclass
puts 4.class.class.superclass.superclass.superclass
puts 4.class.class.superclass.superclass.superclass.superclass

class Tree 
    attr_accessor :children, :node_name
    def initialize(name, children=[])
        @children = children
        @node_name = name
    end
    def visit_all(&block)
        visit &block
        children.each {|c| c.visit_all &block}
    end
    def visit(&block)
        block.call self
    end
end
ruby_tree = Tree.new("Ruby", [Tree.new("Reia"), Tree.new("MacRuby")])
puts "Visiting a node"
ruby_tree.visit {|node| puts node.node_name}
puts
puts "visiting entire tree"
ruby_tree.visit_all {|node| puts node.node_name}
puts
puts ruby_tree.class

# Writing a Mixin
module ToFile
  def filename
    "object_#{self.object_id}.txt"
  end
  def to_f
    File.open(filename, 'w') {|f| f.write(to_s)}
  end
end
class Person 
    include ToFile
    attr_accessor :name
    def initialize(name)
        @name = name
    end
    def to_s
        name
    end
end
Person.new('matz').to_f

# Modules, Enumerable, and Sets
puts
puts 'begin' <=> 'end'
puts 'end' <=> 'begin'
puts 'same' <=> 'same'

a = [5, 3, 4, 1]
puts a
a.sort
puts a
puts a.sort
puts a.any? {|i| i > 6}
puts a.any? {|i| i > 4}
puts a.all? {|i| i > 4}
puts a.all? {|i| i > 0}
puts a.collect {|i| i * 2}
puts a.select {|i| i % 2 == 0} # even numbers
puts a.select {|i| i % 2 == 1} # odd numbers
puts a.max
puts a.member?(2)
puts a.map {|i| i * 2} # same result as collect
puts a.find {|i| i % 2 == 1} # finds first odd number
puts a.find_all {|i| i % 2 == 1} # finds all odd numbers (as select does)
puts
puts a.inject(0) {|sum, i| sum + i}
puts a.inject {|sum, i| sum + i}
puts a.inject {|product, i| product * i}
a.inject(0) do |sum, i|
    puts "sum: #{sum} i: #{i} sum + i: #{sum + i}"
    sum + i 
end
