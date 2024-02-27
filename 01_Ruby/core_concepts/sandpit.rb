# Local variables
name = "Ted"
age = 25
_temp = 100

# Instance variables
class Person
  def initialize(name, age)
    @name = name        # An instance variable named @name with the value of the name parameter
    @age = age          # An instance variable named @age with the value of the age parameter
  end

  def greet
    puts "Hello, my name is #{@name} and I am #{@age} years old."
  end
end

alice = Person.new("Ted", 25)     # A new instance of the Person class with the name and age arguments
alice.greet                         # Invokes the greet method of the alice object

# Class variables
class Animal
  @@count = 0               # A class variable named @@count with the initial value of 0

  def initialize(name)
    @name = name            # An instance variable named @name with the value of the name parameter
    @@count += 1            # Increments the class variable @@count by 1
  end

  def self.count            # A class method named count that returns the value of the class variable @@count
    @@count
  end
end

cat = Animal.new("Cat")     # A new instance of the Animal class with the name argument
dog = Animal.new("Dog")     # Another new instance of the Animal class with the name argument
puts Animal.count           # Prints the value of the class variable @@count, which is 2

# Global variables
$greeting = "Hello"         # A global variable named $greeting with the value "Hello"

def say_hello(name)
  puts "#{$greeting}, #{name}!" # Uses the global variable $greeting and the local variable name to print a message
end

say_hello("Ted")                # Invokes the say_hello method with the name argument

# Constants
PI = 3.14                       # A constant named PI with the value 3.14
puts PI                         # Prints the value of the constant PI


# if...else statement
x = 10
if x > 0
  puts "x is positive"
else
  puts "x is negative or zero"
end

# elsif statement
grade = 85
if grade >= 90
  puts "You got an A"
elsif grade >= 80
  puts "You got a B"
elsif grade >= 70
  puts "You got a C"
else
  puts "You need to study more"
end

# unless statement
y = -5
unless y >= 0
  puts "y is negative"
end

# how it would look as an if statement
if not y >= 0
  puts "y is negative"
end
  

# case...when statement
z = 3
case z
when 1
  puts "z is one"
when 2
  puts "z is two"
when 3
  puts "z is three"
else
  puts "z is something else"
end

# ternary statement
a = 7
b = a > 5 ? "a is big" : "a is small"
puts b

# nested if statement
weather = "sunny"
temperature = 25
if weather == "sunny"
  puts "It's a nice day"
  if temperature > 20
    puts "You should wear sunscreen"
  else
    puts "You should wear a jacket"
  end
else
  puts "It's a cloudy day"
  if temperature < 10
    puts "You should wear a coat"
  else
    puts "You should wear a sweater"
  end
end

# while loop
bark = 4
while bark >= 1
  puts "Woof!"
  bark = bark - 1
end

# for in loop – over a range 1 to 5 inclusive for i
for i in 1..5
  puts i
end

# Integer.times do loop – this will start at 0 and end at 4
5.times do |i|
  puts i
end

# for in loop – over an array for each item
array = ["one", "two", "three", "four", "five"]
for item in array
  puts item
end

# do loop with a break controlled by an if statement
loop do 
  puts "What is your command (or enter stop)?"
  x = gets.chomp
  # ..... do some stuff
  break if x == "stop"
end 

# There is a begin...while in Ruby
begin
  puts "What can I do for you (or enter stop)?"
  x = gets.chomp
  # ..... do some stuff
end while x != "stop"

# until loop
bark = 4
until bark < 1
  puts "Woof!"
  bark = bark - 1
end

# each method - applies a block of code to each element
array = [100, 200, 300, 400, 500]
array.each { |x| puts x }

# map method - applies a block of code to each element and returns a new collection with the modified elements
array = [1, 2, 3, 4, 5]
new_array = array.map { |x| x * 2 }
puts new_array

# select method - applies a block of code to each element and returns a new collection with the selected elements
array = [1, 2, 3, 4, 5]
even_array = array.select { |x| x.even? }
puts even_array

# Define two variables
x = 10
y = 5

# Use arithmetic operators to perform calculations
puts x + y          # 15
puts x - y          # 5
puts x * y          # 50
puts x / y          # 2
puts x % y          # 0
puts x ** y         # 100000

# Use comparison operators to compare values
puts x == y         # false
puts x != y         # true
puts x > y          # true
puts x < y          # false
puts x >= y         # true
puts x <= y         # false

# Use assignment operators to assign or modify values
x = y               # x is now 5
puts x
x += y              # x is now 10
puts x
x -= y              # x is now 5
puts x
x *= y              # x is now 25
puts x
x /= y              # x is now 5
puts x
x %= y              # x is now 0
puts x
x **= y             # x is now 0
puts x

# Use logical operators to combine boolean values
puts true && false  # false
puts true || false  # true
puts !true          # false
puts !false         # true

# Use bitwise operators to manipulate bits
puts 10 & 5         # 0
puts 10 | 5         # 15
puts 10 ^ 5         # 15
puts ~10            # -11
puts 10 << 2        # 40
puts 10 >> 2        # 2

# Use miscellaneous operators for other purposes
puts x == 10 ? "x is 10" : "x is not 10"    # x is not 10
puts ["a", "b", "c"][0]                     # a
for i in 1..5                               # 1..5 so i will be inclusive to the range 1 to 5
  puts i
end
for i in 1...5                              # 1...5 so i will be in the range 1 to 4 with the exlusion of 5
  puts i
end

# This is a single-line comment
puts "Hello, world!" # You can put comments here


# This is a block comment
# It explains how the following loop works
numbers = [1, 2, 3, 4, 5]
numbers.each do |n|
  # This is an inline comment
  puts n ** 2 # Here is another comment 
end

=begin
This is a multiline comment
It can contain any text or symbols
But it cannot be nested or indented
=end

# Define an abstract class for animals
class Animal
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def sound
    "Generic animal sound"
  end

  def move
    "Generic animal movement"
  end
end

# Define a subclass for dogs
class Dog < Animal
  def sound
    "Woof"
  end

  def move
    "Run"
  end
end

# Define a subclass for cats
class Cat < Animal
  def sound
    "Meow"
  end

  def move
    "Walk"
  end
end

# Define a subclass for birds
class Bird < Animal
  def sound
    "Chirp"
  end

  def move
    "Fly"
  end
end

# Create some animal objects
dog = Dog.new("Spot", 3)
cat = Cat.new("Fluffy", 2)
bird = Bird.new("Tweety", 1)

# Call the sound and move methods on each object
puts dog.sound  # Woof
puts dog.move   # Run
puts cat.sound  # Meow
puts cat.move   # Walk
puts bird.sound # Chirp
puts bird.move  # Fly


# Define a module for drivable vehicles
module Drivable
  def start
    "Start the engine"
  end

  def stop
    "Stop the engine"
  end

  def accelerate
    "Increase the speed"
  end

  def brake
    "Decrease the speed"
  end
end

# Define a module for flyable vehicles
module Flyable
  def take_off
    "Lift off the ground"
  end

  def land
    "Touch down on the ground"
  end

  def ascend
    "Go higher in the air"
  end

  def descend
    "Go lower in the air"
  end
end

# Define a class for cars
class Car
  # Include the Drivable module
  include Drivable
end

# Define a class for trucks
class Truck
  # Include the Drivable module
  include Drivable
end

# Define a class for motorcycles
class Motorcycle
  # Include the Drivable module
  include Drivable
end

# Define a class for planes
class Plane
  # Include both the Drivable and Flyable modules
  include Drivable
  include Flyable
end

# Create some vehicle objects
car = Car.new
truck = Truck.new
motorcycle = Motorcycle.new
plane = Plane.new

# Call the methods of the Drivable module on the car object
puts car.start # Start the engine
puts car.accelerate # Increase the speed
puts car.brake # Decrease the speed
puts car.stop # Stop the engine

# Call the methods of the Drivable module on the truck object
puts truck.start # Start the engine
puts truck.accelerate # Increase the speed
puts truck.brake # Decrease the speed
puts truck.stop # Stop the engine

# Call the methods of the Drivable module on the motorcycle object
puts motorcycle.start # Start the engine
puts motorcycle.accelerate # Increase the speed
puts motorcycle.brake # Decrease the speed
puts motorcycle.stop # Stop the engine

# Call the methods of both the Drivable and Flyable modules on the plane object
puts plane.start # Start the engine
puts plane.take_off # Lift off the ground
puts plane.ascend # Go higher in the air
puts plane.descend # Go lower in the air
puts plane.land # Touch down on the ground
puts plane.stop # Stop the engine


# Define a method for calculating the factorial of a number
def factorial(n)
  # Initialize a variable to store the result
  result = 1
  # Loop from 1 to n
  (1..n).each do |i|
    # Multiply the result by i
    result *= i
  end
  # Return the result
  result
end

# Call the factorial method with different arguments
puts factorial(5) # 120
puts factorial(10) # 3628800
puts factorial(15) # 1307674368000
puts factorial(1) # 1
puts factorial(0) # 1
puts factorial(-5) # 1

# We could use recursion
def r_factorial(n)
  # If n is zero or one, return 1
  return 1 if n <= 1
  # Otherwise, multiply n by the factorial of n-1
  n * r_factorial(n-1)
end

puts r_factorial(5) # 120

def i_factorial(n)
  # Use 1 as the initial value and multiply it by each element from 1 to n
  (1..n).inject(1) {|result, i| result * i}
end

puts i_factorial(5) # 120


my_var = "Hello"                # my_var is a string
my_var = 42                     # my_var is now an integer (and we can check using the class method)
puts my_var.class
my_var = "The answer is?"       # my_var is a string again
puts my_var.class

# There are other methods such as is_? that can be used to check types
my_string = "Ruby"
my_array = [1, 2, 3]
puts my_string.is_a?(String)    # prints true
puts my_array.is_a?(Object)     # prints true
puts my_array.is_a?(Array)      # prints true
puts my_array.is_a?(String)     # prints false
puts my_array[0].is_a?(Numeric) # prints true

my_bool = false
puts my_bool
puts my_bool.class
my_bool = nil
puts my_bool
puts my_bool.class

# You can create enumerators in the following way - there are other ways - we will revist this subject later
# But we will take a quick look at what is created
enumerator = %w(one two three).each
puts
puts enumerator
puts enumerator.class

enumerator.each do |item, obj|
    puts "#{item}"
end

# Pointers are not a native data type in Ruby, unlike some other languages like C or C++.
# However, Ruby uses references. 
# In the following example, a and b are both references to the same string object. 
# When you modify the object through a, you also affect b, because they point to the same memory location. 
# This is similar to how pointers work in C or C++.
# However, references are not exactly the same as pointers. 
# References are abstracted from the user, meaning that you cannot directly access or manipulate the 
# memory address of the object. You cannot perform pointer arithmetic or dereference the reference. 
# References are also managed by the Ruby interpreter, which handles memory allocation and garbage collection 
# for the objects. You do not have to worry about freeing the memory or creating memory leaks.

a = "Hello"         # a is a reference to a string object
b = a               # b is another reference to the same object
a.upcase!           # modifies the object in place
puts
puts a              # prints HELLO
puts b              # prints HELLO

# Lets run with this 
b.downcase!
puts
puts a              # prints hello
puts b              # prints hello
c = b
c.upcase!
puts
puts a              # prints HELLO
puts b              # prints HELLO
puts c              # prints HELLO

d = c.dup
d.downcase!
puts
puts a              # prints HELLO
puts b              # prints HELLO
puts c              # prints HELLO
puts d              # prints hello

# but you might want to do a bit of homework on the use of the dup (or clone) method as they could have some 
# unwanted side-effects with certain objects
# of course you could try this, but even though a2 and b2 start out different b2 = a2 creates a reference and....
a2 = "Hello"
b2 = ""
b2 = a2
a2.upcase!
puts
puts a2              # prints HELLO
puts b2              # prints HELLO

# further information on integer type prefixes you can use if you need / want to
# decimal - 0d or 0D
# octal - 0o or 0O or 0
# hexadecimal - 0x or 0X
# binary - 0b or 0B 
# float - numE1

oct = 0o13425
flt = 1.234E4
puts
puts oct
puts oct.class
puts flt
puts flt.class

# We will be talking more about ranges later but as a teaser here is something you can do in Ruby
range = 1..10
puts
puts range
puts range.class

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

# Hash: A hash is a collection of key-value pairs that can be of any type. 
# You can use hashes to store mappings between keys and values, such as dictionaries, phone books, or configurations.
# Create a hash of countries and their capitals
countries = {"Canada" => "Ottawa", "France" => "Paris", "Japan" => "Tokyo"}
# Access the value by key
puts countries["France"] # => "Paris"
# Add a new key-value pair
countries["Brazil"] = "Brasilia"
# Delete a key-value pair
countries.delete("Japan") # => "Tokyo"
# Iterate over the hash
countries.each do |key, value|
  puts "#{key} has #{value} as its capital"
end

# Set: A set is a collection of unordered, unique elements that can be of any type. 
# You can use sets to store sets of items, check for membership, perform set operations, and eliminate duplicates.
# Create a set of colors
colors = Set.new(["red", "green", "blue"])
puts colors
# Check if an element is in the set
puts colors.include?("yellow") # => false
# Add a new element to the set
colors.add("yellow")
puts colors
# Remove an element from the set
colors.delete("red")
puts colors
# Perform set operations
puts colors.union(Set.new(["pink", "purple"]))           # => Set: {"green", "blue", "yellow", "pink", "purple"}>
puts colors
puts colors.intersection(Set.new(["green", "orange"]))   # => Set: {"green"}>
puts colors
puts colors.difference(Set.new(["blue", "yellow"]))      # => Set: {"green"}>
puts colors

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
age = 36
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
