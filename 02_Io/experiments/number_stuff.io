/*
The NumbersList prototype is an object designed to manage a collection of numerical values. 
It provides a suite of methods for performing statistical calculations and manipulations on a list of numbers. 
Users can interact with the prototype to add numbers, view them, and calculate various statistical measures.

Methods:
- promptAndAddNumbers: Prompts the user to enter numbers into the list. Accepts numerical input until an 
  empty line is entered.
- display: Prints the current list of numbers to the console.
- appendNumber: Adds a single number to the list. Validates the input to ensure it is a number.
- sum: Calculates and returns the sum of all numbers in the list.
- average: Computes the average (mean) value of the numbers in the list.
- product: Calculates the product of all numbers in the list.
- minimum: Finds and returns the smallest number in the list.
- maximum: Finds and returns the largest number in the list.
- variance: Calculates the variance of the numbers in the list.
- standardDeviation: Computes the standard deviation of the numbers, a measure of the dispersion.
- sortAscending: Sorts the numbers in the list in ascending order.
- sortDescending: Sorts the numbers in the list in descending order.
- median: Determines and returns the median value of the numbers.
- meanAbsoluteDeviation: Calculates the mean absolute deviation, a measure of the average distance 
  between each data point and the mean.

Usage:
To use the NumbersList prototype, clone it and invoke the promptAndAddNumbers method to begin entering numbers. 
After populating the list, you can call any of the provided methods to perform statistical analysis on your data.

Example:
numbers := NumbersList clone
numbers promptAndAddNumbers()
numbers display()
writeln("Sum of numbers: " .. numbers sum())
// ... additional method calls ...

Note:
This prototype assumes that all input is numerical and does not handle all other data types. 
Although, it does catch for NAN on the input conversion to a number. For example: a13 will be caught as 
being not a valid number, but 13a and 13,2 will be converted automatically to 13 (it will take the first number 
part of the entry where it is followed by a non-numeric unless it is a single . as in 13.2, which will
be parsed as 13.2 and compare this to 13.2.4 will be parsed as 13.2). 

So, ensure that the input is cleaned or validated before using these methods if there's a possibility of non-numeric data.
*/


// Create a prototype for managing a list of numbers
NumbersList := Object clone do(
    // Initialize the list
    list := List clone

    // Method to prompt user and add numbers to the list
    promptAndAddNumbers := method(
        loop(
            userInput := File standardInput readLine("Enter a number (or just press enter to finish): ")
            if(userInput isEmpty, break)
            if((userInput asNumber) isNan, 
                "Not a valid number. Please try again." println,
                self appendNumber(userInput asNumber)                
            )
        )
    )

    // Method to display all numbers in the list
    display := method(
        "Your numbers: " print
        list println
    )

    // Method to add a number to the list
    appendNumber := method(number, 
        if(number isNan, 
            "Not a valid number." println,
            list append(number)
        )
    )

    // Method to sum all numbers in the list
    sum := method(
        list reduce(+, 0)
    )

    // Method to find the average of the numbers
    average := method(
        if(list size > 0, list sum / list size, 0)
    )

    // Method to calculate the product of all numbers
    product := method(
        list reduce(*, 1)
    )

    // Method to find the minimum number in the list
    minimum := method(
        list min
    )

    // Method to find the maximum number in the list
    maximum := method(
        list max
    )

    // Method to calculate the variance of the numbers
    variance := method(
        mean := self average()
        list map(n, (n - mean) ** 2) reduce(+, 0) / list size
    )

    // Method to calculate the standard deviation of the numbers
    standardDeviation := method(
        self variance sqrt
    )

    // Method to sort the numbers in ascending order
    sortAscending := method(
        list sort
    )

    // Method to sort the numbers in descending order
    sortDescending := method(
        list sort reverse
    )

    // Method to calculate the median of the numbers
    median := method(
        sortedList := self sortAscending()
        size := sortedList size
        if(size % 2 == 0,
            // Even number of elements, average the middle two
            (sortedList at(size / 2 - 1) + sortedList at(size / 2)) / 2,
            // Odd number of elements, take the middle
            sortedList at(size / 2)
        )
    )

    // Method to calculate the Mean Absolute Deviation of the numbers
    meanAbsoluteDeviation := method(
        mean := self average()
        deviations := list map(n, (n - mean) abs)
        deviations sum / list size
    )
)

// Use the NumbersList prototype
numbers := NumbersList clone
numbers promptAndAddNumbers()
numbers display()
writeln("Sum of numbers: " .. numbers sum())
writeln("Average of numbers: " .. numbers average())
writeln("Product of numbers: " .. numbers product())
writeln("Minimum of numbers: " .. numbers minimum())
writeln("Maximum of numbers: " .. numbers maximum())
writeln("Variance of numbers: " .. numbers variance())
writeln("Standard Deviation of numbers: " .. numbers standardDeviation())
writeln("Ascending Sort of numbers: " .. numbers sortAscending())
writeln("Descending Sort of numbers: " .. numbers sortDescending())
writeln("Median of numbers: " .. numbers median())
writeln("Mean Absolute Deviation of numbers: " .. numbers meanAbsoluteDeviation())

