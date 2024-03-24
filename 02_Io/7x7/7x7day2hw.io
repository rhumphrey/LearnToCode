// Day 2 Self-Study (Homework)
// Do:
// 1. A Fibonacci sequence starts with two 1s. Each subsequent number
// is the sum of the two numbers that came before: 1, 1, 2, 3,
// 5, 8, 13, 21, and so on. Write a program to find the nth Fibonacci
// number. fib(1) is 1, and fib(4) is 3. As a bonus, solve the problem
// with recursion and with loops.
//
// 2. How would you change / to return 0 if the denominator is zero?
//
// 3. Write a program to add up all of the numbers in a two-dimensional
// array.
//
// 4. Add a slot called myAverage to a list that computes the average of
// all the numbers in a list. What happens if there are no numbers
// in a list? (Bonus: Raise an Io exception if any item in the list is not
// a number.)
//
// 5. Write a prototype for a two-dimensional list. The dim(x, y) method
// should allocate a list of y lists that are x elements long. set(x,y,
// value) should set a value, and get(x, y) should return that value.
//
// 6. Bonus: Write a transpose method so that (new_matrix get(y, x)) ==
// matrix get(x, y) on the original list.
//
// 7. Write the matrix to a file, and read a matrix from a file.
//
// 8. Write a program that gives you ten tries to guess a random number
// from 1–100. If you would like, give a hint of “hotter” or “colder”
// after the first guess.

//
// 1. Fibonacci
// Recursive Approach
fibRecursive := method(n,
    if(n <= 2, return 1)
    fibRecursive(n - 1) + fibRecursive(n - 2)
)
// Example usage:
fibRecursive(1) println // Output: 1
fibRecursive(4) println // Output: 3
"" println
//
// Iterative Approach with Loops
fibIterative := method(n,
    a := 1
    b := 1
    for(i, 1, n - 1,
        a, b = b, a + b
    )
    a
)
// Example usage:
fibIterative(1) println // Output: 1
fibIterative(4) println // Output: 3
"" println

//
// 2. Change / to return 0 if the denominator is zero?
// There are a couple of ways you can create a so called 'safe divide' in Io
// By creating a safe divide method
safeDivide := method(numerator, denominator,
    if(denominator == 0, return 0)
    numerator / denominator
)
// Example usage:
safeDivide(10, 2) println  // Output: 5
safeDivide(10, 0) println  // Output: 0
//
// Or, by overriding the divison operator
// First, before we do any overiding we are going to store the original division method in a slot
origDiv := Number getSlot("/")
// Redefine the division method
Number setSlot("/", method(i,
    if(i == 0, 
        0, 
        self origDiv(i)
    )
))
// Example usage:
(10 / 5) println  // Output: 2
(10 / 0) println  // Output: 0
"" println

//
// 3. Write a program to add up all of the numbers in a two-dimensional array.
sum2DArray := method(array,
    total := 0
    array foreach(subArray,
        subArray foreach(number,
            total = total + number
        )
    )
    total
)
// Sample usage:
twoDimensionalArray := list(
    list(1, 2, 3),
    list(4, 5, 6),
    list(7, 8, 9)
)
sum := sum2DArray(twoDimensionalArray)
"Sum of all numbers in the two-dimensional array is: " print
sum println
"" println

// 
// 4. myAverage 
List myAverage := method(
    if(self size == 0,
        return "List is empty"         
    ) // I replaced this so code would continue to run - Exception raise("List is empty") goes here instead of the return "List is empty"
    self foreach(x, 
        if(x type != "Number",  
            return "Non-number element found"
        ) // - Exception raise("Non-number element found")
    )
    self sum / self size
)
// Example usage:
numbers := list(1, 2, 3, 4, 5)
numbers println
// numbers average println          // built in method for testing
numbers myAverage println           // Output: 3
emptyList := list()
emptyList myAverage println         // Raises Exception: "List is empty"
mixedList := list(1, 2, "three", 4, 5)
mixedList myAverage println         // Raises Exception: "Non-number element found"
"" println

//
// 5. prototype for two dimensional list with dim, set and get methods
TwoDimensionalList := List clone do(
    dim := method(x, y,
        self list := List clone
        for(i, 0, x - 1,
            row := List clone
            for(j, 0, y - 1, row append(0))  // Initialize all elements to 0
            self list append(row)
        )
    )
    set := method(x, y, value, self list at(x) atPut(y, value))
    get := method(x, y, self list at(x) at(y))
)

// Example usage:
matrix := TwoDimensionalList clone
matrix dim(3, 3)  // Create a 3x3 two-dimensional list
matrix set(0, 0, 1)
matrix set(0, 2, 8)
matrix set(1, 1, 2)
matrix set(2, 2, 3)
matrix get(0, 0) println  // Output: 1
matrix get(1, 1) println  // Output: 2
matrix get(2, 2) println  // Output: 3

// method to easily print the matrix
TwoDimensionalList printMatrix := method(
    for(i, 0, self list size - 1,
        for(j, 0, self list at(i) size - 1,
            write(self get(i, j), " ")
        )
        writeln("")  // New line for each row
    )
)
matrix printMatrix
"" println

// 
// 6. Bonus - Transpose method
TwoDimensionalList transpose := method(
    transposed := TwoDimensionalList clone
    transposed dim(self list at(0) size, self list size)  // Initialize transposed matrix with dimensions swapped
    for(i, 0, self list size - 1,
        for(j, 0, self list at(i) size - 1,
            transposed set(j, i, self get(i, j))  // Swap row and column indices
        )
    )
    transposed  // Return the transposed matrix
)
// Example usage:
matrix2 := TwoDimensionalList clone
matrix2 dim(2, 3)
matrix2 set(0, 0, 1)
matrix2 set(0, 1, 2)
matrix2 set(0, 2, 3)
matrix2 set(1, 0, 4)
matrix2 set(1, 1, 5)
matrix2 set(1, 2, 6)
transposedMatrix := matrix2 transpose
matrix2 printMatrix
transposedMatrix printMatrix
"" println

//
// 7. Matrix to file then matrix from file
// Method to write a matrix to a file
TwoDimensionalList writeToFile := method(filename,
    file := File with(filename)
    file openForUpdating
    for(y, 0, list size - 1,
        for(x, 0, list at(y) size - 1,
            file write(list at(y) at(x) asString)
            if(x != list at(y) size - 1, file write(" "))
        )
        file write("\n")
    )
    file close
)
// Method to read a matrix from a file - use the next method as the prefered solution
TwoDimensionalList readFromFile := method(filename,
    file := File with(filename)
    list := List clone
    file openForReading
    content := file contents
    file close
    content split("\n") foreach(line,
        list append(line split(" ") map(asNumber))
    )
    list
)
// Another method to write a matrix from a file - prefered solution for this problem
TwoDimensionalList readMatrixFromFile := method(filename,
    // Open the file and read its contents then close
    file := File with(filename)
    lines := file readLines
    file close                  
    // Determine the dimensions of the matrix
    rowCount := lines size
    colCount := lines at(0) split(" ") size
    // Initialize the matrix with the determined dimensions
    self dim(rowCount, colCount)
    // Iterate over each line and each value to populate the matrix
    for(i, 0, rowCount - 1,
        values := lines at(i) split(" ")
        for(j, 0, colCount - 1,
            self set(i, j, values at(j) asNumber)
        )
    )
    return self 
)

// Example usage:
// Write the matrix to a file
matrix writeToFile("matrix.txt")
// Read a matrix from a file
newMatrix := TwoDimensionalList readFromFile("matrix.txt")
otherMatrix := TwoDimensionalList readMatrixFromFile("matrix.txt")
//
// experiment with a standalone method for printing matrix due to error messages with the TwoDimensionalList printing method
printFileMatrix := method(matrix,
    matrix foreach(row,
        row foreach(element,
            write(element asString .. " ")
        )
        writeln("")  // This creates a new line after printing each row
    )
)
// Testing the different read methods - they need different print methods 
// Using the one in the TwoDimensionalList object
matrix printMatrix
"" println
otherMatrix printMatrix // for matrix from TwoDimensionalList readMatrixFromFile("matrix.txt")
"" println
// using the standalone print method for matrix from TwoDimensionalList readFromFile("matrix.txt")
printFileMatrix(newMatrix)
"" println


//
// 8. Guessing game 
// Guessing Game in Io
randomNumber := Random value(1, 100) floor
previousDifference := nil

"Guess a number between 1 and 100. You have ten tries." println

for(i, 1, 10,
    guess := File standardInput readLine asNumber
    difference := (guess - randomNumber) abs
        
    if(guess == randomNumber,
        ("Congratulations! You've guessed the correct number: " .. randomNumber .. "!") println
        break
    )
    
    if(previousDifference != nil,
        if(difference < previousDifference,
            "Hotter" println,
            "Colder" println
        )
    )
    
    if(i == 10,
        ("Game over! The correct number was " .. randomNumber .. ".") println,
        ("Try again. This was attempt " .. i .. " of 10.") println
    )
    
    previousDifference = difference
)


