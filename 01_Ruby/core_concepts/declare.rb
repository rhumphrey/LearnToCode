# Declarations and types
# Ruby is a dynamically typed language, which means that you don’t have to declare the types of your variables 
# before you assign values to them. The type of a variable is determined by the value it holds at runtime, 
# and it can change as you assign different values. 
# For example, you can assign a string to a variable, and then assign a number to the same variable later

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

# So what types are there in Ruby?
# Numbers
# Boolean
# String
# Hashes
# Array
# Symbol

# Lets look at how these appear when we put them vs the class. Later we will spend more time on use cases
# and look deeper at data structures 
my_int = 1
my_float = 1.0
my_num1 = 1/2
my_num2 = 1/2.0
my_bool = true
my_string = "Boat"
my_hash = {name: "Max", age: 20}
my_array = [10, 20, 30, 40, 50]
my_symbol = :hello

puts
puts my_int
puts my_int.class
puts my_float
puts my_float.class
puts my_num1
puts my_num1.class
puts my_num2
puts my_num2.class
puts my_bool
puts my_bool.class
puts my_string
puts my_string.class
puts my_hash
puts my_hash.class
puts my_array
print my_array
puts
puts my_array.class
puts my_symbol
puts my_symbol.class
puts
my_bool = false
puts my_bool
puts my_bool.class
my_bool = nil
puts my_bool
puts my_bool.class

# Experiment with the above vs various other type related methods

# You can create enumerators in the following way - there are other ways - we will revist this subject later
# But we will take a quick look at what is created
enumerator = %w(one two three).each
puts
puts enumerator
puts enumerator.class

enumerator.each do |item, obj|
    puts "#{item}"
end

# What about pointers?
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

# Variable naming and scope/type is connected in Ruby and we will look at some of these in more detail
# Character   Type of Variable
# [a-z] or _	Local Variable
# @	          Instance Variable
# @@	        Class Variable
# $	          Global Variable
