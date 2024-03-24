# Simple I/O example in Io
# Ask the user for input
userInput := File standardInput readLine("Please enter something: ")

# Print the input back to the user
"Your input was: " .. userInput println
# Print a success message
"If you see your input echoed back, Io is working correctly." println