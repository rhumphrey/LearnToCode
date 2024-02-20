# Day 1 experiments
properties = ['object oriented', 'duck typed', 'productive', 'fun']
properties.each {|property| puts "Ruby is #{property}."}

properties.each {|property| puts "Ruby is #{property}."}

# outputs, variables, dynamic typing and variables as objects
puts 'Hello, world'

language = 'Ruby'
puts "Hello, #{language}"

language = 'Ruby! Ruby! Ruby!'
puts "Hello, #{language}"

language = '154'
puts "Hello, #{language}"

puts "Hello, #{'Room: ' + language}"
puts 4

puts 4.class

puts 4 / 2
puts 4 / 2.0

puts (4 / 2.0).class

language = 'Ruby'
puts language.class

language = 1
puts language.class

language = 1.0
puts language.class

language = nil
puts language.class

language = true
puts language.class

language = false
puts language.class

language = "Ruby"
puts language.class
# puts language.methods

# decisions and conditionals
puts x = 4
puts x 

puts x < 5
puts x <= 4
puts x > 4

puts false.class
puts true.class

puts 'This appears to be false.' unless x == 4
puts 'This appears to be true.' if x == 4

if x == 4
    puts 'This appears to be true'
end

unless x == 4
    puts 'This appears to be false.'
else
    puts 'This appears to be true.'
end

puts 'This apears to be true.' if not true
puts 'This apears to be true.' if ! true

puts 'This apears to be true.' if not x == 4
puts 'This apears to be true.' if ! x == 4

# conditionals in thr context of while and until
x = x + 1 while x < 10
puts x

x = x -1 until x == 1
puts x

while x < 10
    x = x + 1
    puts x
end

# more true and false stuff
puts 'This appears to be true - 1' if 1
puts 'This appears to be true - 0' if 0
puts 'This appears to be true - true' if true
puts 'This appears to be true - false' if false
puts 'This appears to be true - nil' if nil
puts 'This appears to be true - word' if 'word'

# logical operators
puts true and false
puts true or false
puts false && false
puts false || false
puts false & true
puts false | true

# strong dynamic duck typing and coercsion / conversion
# puts 4 + 'four' # this causes an error - String can't be coerced into Integer (TypeError)
# these chacks are done at run time

i = 0
a = ['100', 100.0]
puts a.class
puts a[0].class
puts a[1].class
while i < 2
    puts a[i].to_i
    i = i + 1
end

puts a[0].to_i + a[1]
puts a[0].to_i + a[1].to_i
