# Ruby is an interpreted, high-level, general-purpose programming language that supports multiple programming paradigms, such as procedural, object-oriented, 
# and functional programming. It was designed by Yukihiro Matsumoto in Japan in the mid-1990s, with a focus on simplicity and productivity. 
# In Ruby, everything is an object, including primitive data types, and you can use many scripting features to process text, files, or system tasks. 
# Ruby is also widely used for web development, especially with the Ruby on Rails framework.

# An example of some ruby code
# This program asks the user for their name and greets them
puts "Please enter your name."      # puts is a method that prints a string to the screen
name = gets.chomp                   # gets is a method that reads a line of input from the user and chomp is a method that removes the newline character at the end
puts "Hello, #{name}! I'm Ruby!"    # #{name} is an expression that interpolates the value of the name variable into the string

# Remember - everything is an object
puts
puts "Remember - in Ruby everything is an object"
gets
puts name.class 
puts
puts name.methods.sort

# Useful Ruby Links
# Ruby Programming Language. https://www.ruby-lang.org/en/
# Help and documentation for the Ruby programming language - https://ruby-doc.org/
# The Ruby API - https://rubyapi.org/
# Ruby (programming language) - Wikipedia. https://en.wikipedia.org/wiki/Ruby_%28programming_language%29
# Ruby Conference 2008 Reasons behind Ruby by Yukihiro 'Matz' Matsumoto - https://www.youtube.com/watch?v=vDnOBXD167k