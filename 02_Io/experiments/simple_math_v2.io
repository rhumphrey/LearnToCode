// Experimenting with objects, messages and simple math for two numbers
// First refactoring

/*
Title: TwoValues Calculator
Description: This Io program defines a prototype named 'TwoValues' that allows users to perform various 
mathematical operations on two numbers. The user is prompted to input two whole numbers, 
and the program calculates and displays the sum, average, maximum, difference, product, quotient, power, 
modulus, minimum, geometric mean, harmonic mean, and Euclidean distance between the two numbers.

Usage:
1. The user is prompted to enter the first and second whole number when the program runs.
2. The program then performs calculations using these numbers and displays the results.

Features:
- Dynamic input: Users can input different numbers each time they run the program.
- Multiple operations: The program can calculate and display a variety of mathematical results.
- Error handling: The program includes a check to prevent division by zero.

Note: This program is an example of object-oriented programming in Io, demonstrating cloning, message passing, 
and method definition.

Version: 1.0
*/

// Creating a new prototype called TwoValues by cloning the base Object prototype
TwoValues := Object clone do(
    // Initializing slots to store the two numbers
    firstNumber := nil
    secondNumber := nil

    // Defining methods to set the first and second number
    setFirstNumber := method(n, firstNumber = n; self)
    setSecondNumber := method(n, secondNumber = n; self)

    // Method to read a number from the standard input
    readNumber := method(prompt,
        ("Please enter " .. prompt .. ": ") print;
        File standardInput readLine asNumber
    )

    // Method to calculate various mathematical operations
    sum := method(firstNumber + secondNumber)
    average := method(sum / 2)
    maximum := method(if(firstNumber > secondNumber, firstNumber, secondNumber))
    difference := method(if(firstNumber > secondNumber, firstNumber - secondNumber, secondNumber - firstNumber))
    product := method(firstNumber * secondNumber)
    quotient :=  method(if(secondNumber != 0, firstNumber / secondNumber, "Cannot divide by zero"))
    power := method(firstNumber ^ secondNumber)
    modulus := method(firstNumber % secondNumber)
    minimum := method(if(firstNumber < secondNumber, firstNumber, secondNumber))
    geometricMean := method((firstNumber * secondNumber) sqrt)
    harmonicMean := method(2 / (1 / firstNumber + 1 / secondNumber))
    euclideanDistance := method((firstNumber - secondNumber) abs)
)

// Creating an instance of TwoValues by cloning the prototype
myTwoValues := TwoValues clone

// Using the instance to interact with the user and set the two numbers
myTwoValues do(
    setFirstNumber(myTwoValues readNumber("first whole number"))
    setSecondNumber(myTwoValues readNumber("second whole number"))
)

// Using the instance to interact with the user and set the two numbers
writeln("The sum is " .. myTwoValues sum)
writeln("The average is " .. myTwoValues average)
writeln("The highest value is " .. myTwoValues maximum)
writeln("The difference is " .. myTwoValues difference)
writeln("The product is " .. myTwoValues product)
writeln("The quotient is " .. myTwoValues quotient)
writeln("The power is " .. myTwoValues power)
writeln("The modulus is " .. myTwoValues modulus)
writeln("The minimum is " .. myTwoValues minimum)
writeln("The geometric mean is " .. myTwoValues geometricMean)
writeln("The harmonic mean is " .. myTwoValues harmonicMean)
writeln("The Euclidean distance is " .. myTwoValues euclideanDistance)