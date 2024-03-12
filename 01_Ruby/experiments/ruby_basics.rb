# This is a comment that explains the purpose of the file
# You can use the # symbol to start a comment on any line

# You can use require or require_relative to load other Ruby files or libraries
require 'csv'                                               # This loads the built-in CSV library
require_relative 'my_lib'                                   # This loads a file named my_lib.rb in the same directory

# You can use constants to store values that do not change
PI = 3.14                                                   # This is a constant for the mathematical constant pi
VERSION = "1.0.0"                                           # This is a constant for the version of the script

# You can use variables to store values that do change
counter = 0                                                 # This is a variable that counts how many times the script runs
name = "Alice"                                              # This is a variable that stores a name

# You can use the module keyword to create namespaces for your classes and methods
module MyModule
  # You can use the class keyword to define classes
  class MyClass
    # You can use the attr_reader, attr_writer, or attr_accessor methods to create getters and setters for instance variables
    attr_reader :name                                       # This creates a getter method for the @name instance variable
    attr_writer :age                                        # This creates a setter method for the @age instance variable
    attr_accessor :gender                                   # This creates both a getter and a setter method for the @gender instance variable

    # You can use the initialize method to define the constructor for the class
    def initialize(name, age, gender)
      # You can use the @ symbol to indicate instance variables
      @name = name                                          # This assigns the name parameter to the @name instance variable
      @age = age                                            # This assigns the age parameter to the @age instance variable
      @gender = gender                                      # This assigns the gender parameter to the @gender instance variable
    end

    # You can use the def keyword to define methods
    def greet
      # You can use the puts method to print output to the standard output
      puts "Hello, #{@name}!"                               # This prints a greeting with the @name instance variable
    end

    # You can use the end keyword to close your classes and methods
  end
end

# You can use the include keyword to include a module in the current scope
include MyModule

# You can use the new method to create an instance of a class
my_object = MyClass.new("Bob", 25, "male")                  # This creates a new object of the MyClass class with the given arguments

# You can use the dot operator to call methods on an object
my_object.greet                                             # This calls the greet method on the my_object object
my_object.age = 26                                          # This calls the age= method on the my_object object to change its age
puts my_object.gender                                       # This calls the gender method on the my_object object and prints its value

# You can use conditional statements to control the flow of your code
if counter == 0                                             # This checks if the counter variable is equal to zero
  puts "This is the first time the script runs."            # This prints a message if the condition is true
elsif counter == 1                                          # This checks if the counter variable is equal to one
  puts "This is the second time the script runs."           # This prints a message if the condition is true
else                                                        # This executes if none of the above conditions are true
  puts "This is the #{counter + 1}th time the script runs." # This prints a message with the counter variable
end

# You can use loops to repeat a block of code
3.times do # This repeats the block three times
  puts "Hello, world!" # This prints a message each time
end

# You can use arrays to store multiple values in a single variable
names = ["Alice", "Bob", "Charlie"]                         # This creates an array with three elements
puts names[0]                                               # This prints the first element of the array
names << "David"                                            # This appends a new element to the end of the array
names.each do |name|                                        # This iterates over each element of the array
  puts "Hello, #{name}!"                                    # This prints a greeting with the element
end

# You can use hashes to store key-value pairs in a single variable
person = {name: "Alice", age: 20, gender: "female"}         # This creates a hash with three key-value pairs
puts person[:name]                                          # This prints the value associated with the key :name
person[:height] = 170                                       # This adds a new key-value pair to the hash
person.each do |key, value|                                 # This iterates over each key-value pair of the hash
  puts "#{key} is #{value}"                                 # This prints the key and the value
end

# You can use the CSV library to read and write CSV files
data = CSV.read("data.csv")                                 # This reads the data.csv file into a two-dimensional array
puts data[0][1]                                             # This prints the second element of the first row
CSV.open("output.csv", "w") do |csv|                        # This opens a new file named output.csv for writing
  csv << ["name", "age", "gender"]                          # This writes a new row with the given elements
  csv << ["Alice", 20, "female"]                            # This writes another new row with the given elements
end

# You can use the methods from the my_lib file
MyLib.hello                                                 # This calls the hello method from the MyLib module
sum = MyLib.add(2, 3)                                         # This calls the add method from the MyLib module with the given arguments
puts sum