puts "Please enter your name:"
user_name = STDIN.gets.not_nil!.strip # Read input, ensure not nil, remove whitespace
puts "Hello, #{user_name}! Welcome to Crystal."