// Define a method 'bottle' that takes a number and returns a string
// based on the number of beer bottles.
bottle := method(number,
    // If the number is 0, return 'no more bottles of beer'.
    if(number == 0, return "no more bottles of beer")
    // If the number is 1, return '1 bottle of beer'.
    if(number == 1, return "1 bottle of beer")
    // For numbers greater than 1, return the number concatenated with
    // ' bottles of beer'.
    return number .. " bottles of beer"
)

// Create a loop that counts down from 99 to 1.
for(number, 99, 1, -1,
    // Call the 'bottle' method and store the result in 'bottles'.
    bottles := bottle(number)
    // Write the current number of bottles on the wall, take one down,
    // and then write the new number of bottles on the wall.
    write(bottles, " on the wall, ", bottles, ".\n",
          "Take one down and pass it around, ",
          bottle(number - 1), " on the wall.\n\n")
)
