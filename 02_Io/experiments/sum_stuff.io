// Ask the user for a list of numbers, separated by spaces
"Enter a list of numbers separated by spaces:" println
input := File standardInput readLine

// Convert the input to an list of values
valueList := input split(" ") 

// Define a variable to hold the sum of the numbers
sum := 0

// Process the array with a loop to calculate the sum - note 
valueList foreach(value, sum = sum + value asNumber)

// Calculate the average
average := sum  / valueList size

// Use control structures to provide feedback
if(average > 10,
   ("The average is greater than 10. It's " .. average) println,
   if(average == 10,
      "The average is exactly 10." println,
      ("The average is less than than 10. It's " .. average) println
   )
)
