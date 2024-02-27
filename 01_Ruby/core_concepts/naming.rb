# Naming conventions - some starting points (other conventions will come up further into the course)
#
# Variables and methods use snake_case, which means all lowercase letters separated by underscores. 
#   For example: first_name, calculate_area, is_valid?.
# Classes and modules use CamelCase, which means each word starts with a capital letter and no spaces or underscores. 
#   For example: String, Math, ActiveRecord.
# Constants use UPPERCASE, which means all capital letters. 
#   For example: PI, MAX_VALUE, ENV.
# Predicate methods, which return a boolean value, end with a question mark. 
#   For example: empty?, include?, valid?.
# Dangerous methods, which modify the receiver or raise an exception, end with a bang. 
#   For example: save!, destroy!, gsub!.
# Private or protected methods start with an underscore. 
#   For example: _initialize, _perform, _validate.

# Variable and method example
first_name = "Alice"                # Variable using snake_case
def say_hello(first_name)           # Method using snake_case
  puts "Hello, #{first_name}!"
end
say_hello(first_name)               # Invoking the method

# Class and module example
class Person                        # Class using CamelCase
  attr_accessor :name, :age         # Instance variables
  def initialize(name, age)         # Constructor method
    @name = name                    # @ prefix for instance variables (we will talk about this and @@ later)
    @age = age
  end
end

module MathStuff                    # Module using CamelCase (of course I could have just called this Math)
  def self.square(x)                # Module method
    x * x
  end
end

puts MathStuff.square(10)

# Constant example
MAX_LENGTH = 100                    # Constant using UPPERCASE
puts "The maximum length is #{MAX_LENGTH}"

# Predicate method example
array = [1, 2, 3]                   # Array object
puts array.empty?                   # Predicate method using question mark
puts array.include?(2)              # Predicate method using question mark

# Dangerous method example
string = "Hello, world!"            # String object
puts string.gsub("world", "Ruby")   # Non-dangerous method
puts string                         # The original string is unchanged
puts string.gsub!("world", "Ruby")  # Dangerous method using bang
puts string                         # The original string is modified

# Private or protected method example
class Animal                        # Class using CamelCase
  def speak                         # Public method
    puts "I am an animal"
    _make_sound                     # Calling a private method
  end

  private                           # Private keyword

  def _make_sound                   # Private method using underscore
    puts "gggrrrrrrr!"
  end
end

animal = Animal.new                 # Creating an instance
animal.speak                        # Calling the public method
animal._make_sound                  # Error: private method called

# We will also see some style conventions as we go along. Maybe you have already noticed that Ruby uses 
# two spaces for indentation, with no tabs. However, this is to add readability, it's not used to indicate
# code blocks to the interpreter. We will see how that's done as we cover the diiferent core features of Ruby. 
# Ruby Style Guide - https://rubystyle.guide/