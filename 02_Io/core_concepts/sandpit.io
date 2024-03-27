// Creating a type-like object
Dog := Object clone
Dog type println // returns "Dog"

// Creating a non-type object from Dog
rover := Dog clone
rover type println // returns "Dog"

// Creating a non-type object from Object
nto := Object clone
nto type println // returns "Object"

"" println

// For the Dog type-like object
Dog breed := "Unknown"  
Dog breed println
rover breed println

// For the rover instance of Dog - notice how I can find out the slotNames
rover breed = "Mutt"
rover age := 3    
Dog breed println
rover breed println
rover slotNames println

// For the nto non-type object
nto description := "a small shiney thing"
nto description println 

// Later I could update the slot
nto description = "a marble"
nto description println 

"" println

// Using our rover object and age slot
if(rover age < 2, "Pupper!", "non-Pupper!") println

// I could also create a slot called isPupper for which I assign conditional as a message
Dog isPupper := if(rover age < 2, "Pupper!", "non-Pupper!")
rover isPupper println

// The compare method
a := 10
b := 20
comparisonResult := a compare(b)
comparisonResult println
Lobby slotNames println
Object slotNames println
Number slotNames println
a type println

// Smalltalk style conditionals
// Notice that the condition expression must have parenthesis surrounding it.
y := 4
x := 6 
z := (y < 10) ifTrue(x = y) ifFalse(x = 2)
// if we explore our curiosity
y println
x println
z println

"" println
// The loop method (with continue and break)
count := 1
loop(
    if(count % 2 == 0, 
        count = count + 1
        continue
    )
    count println
    count = count + 1
    if(count > 10, break)
)

// Repeat
5 repeat("Woof" println)

// I could turn this into a slot on the Dog Object
Dog doingBark := 5 repeat("Woof" println)
rover doingBark

// While
counter := 10
while(counter > 0, 
    ("Counting down: " .. counter) println
    counter = counter - 1
)


// For
// The start and end messages are only evaluated once, when the loop starts.
for(n, 0, 10, n println)
// Example with a step:
for(n, 0, 10, 3, n println)
// Example with a negative step:
for(n, 10, 0, -1, n println)


// break, continue
// see as used in the loop method example previously

// return
// Any part of a block can return immediately using the return method.
testReturn := method("This is done " print; return "This is returned"; "This is not done" println)
testReturn println
// Internally, break, continue and return all work by setting a IoState internal variable called "stopStatus" 
// which is monitored by the loop and message evaluation code.

"" println


// If you want to see the operator table in Io you can use the following, notice the level of precidence is indicated
// those closer to 0 bind first. Notice it also lists the assignment operators and how to add new operators to the table
OperatorTable println
// lets try adding a new operator and setting the precedence (you can also overide the precedence) and then checking the table again
OperatorTable addOperator("xor", 11)
OperatorTable println
// you should see it added on line 11 in the Operators part of the table, with other level 11 operators
// now we just need to implement the xor method on true and false (usimg brute force to keep it simple)
true xor := method(bool, if(bool, false, true))
false xor := method(bool, if(bool, true, false))
// so lets test that xor method
(true xor(true)) println            // true xor true should be false
(true xor(false)) println           // true xor false should be true
(false xor(true)) println           // false xor true should be true
(false xor(false)) println          // false xor false should be false


"" println


// Abstraction - Methods
Dog := Object clone do(
    bark := method(
        "Bark!" println
    )

    promptToFeed := method(
        "Do you want to feed the dog? (yes/no)" println
        answer := File standardInput readLine
        answer asLowercase == "yes"
    )

    barkFiveTimesThenPrompt := method(
        5 repeat(bark)
        if(promptToFeed not, barkFiveTimesThenPrompt)
    )
)

// To use the method:
Dog barkFiveTimesThenPrompt

"" println

// Abstraction - Methods - Factorial
factorial := method(n,
    result := 1
    for(i, 1, n,
        result = result * i
    )
    result
)

factorial(5) println        // 120
factorial(1) println
factorial(0) println
factorial(-5) println

// Abstraction - Methods - Factorial - Recursion
factorial := method(n,
    if(n <= 1,
        return 1
    )
    n * factorial(n - 1)
)

// Usage:
factorial(5) println        // 120
factorial(1) println
factorial(0) println
factorial(-5) println


// Abstraction - Methods - Factorial - using a Range
factorial := method(n,
    if(n == 0, return 1)
    res := 1
    Range 1 to(n) foreach(i, res = res * i)
    res
)
factorial(5) println        // 120
factorial(1) println
factorial(0) println
factorial(-5) println

"" println

// Type checking
value := 3
value type println
value = "Hello" 
value type println
value = list(1,2,3,4,5) 
value type println
value = true
value type println
value = nil
value type println
value = "A"
value type println

"" println

// Strings can be defined surrounded by a single set of double quotes with escaped quotes 
// (and other escape characters) within.
string := "This is a \"test\".\nThis is only a test."
string println
// Or for strings with non-escaped characters and/or spanning many lines, triple quotes can be used.
string := """This is a "test".
This is only a test."""
string println 

"" println

// A List is an array of references and supports all the standard array manipulation and enumeration methods.
newList := List clone                   // Empty list
newList := list(345, "A")               // Using the list method
newList append("Hello")                 // Append an item
newList size println                    // Get the size and print
newList at(1) println                   // Get an item at index - zero indexed
newList atPut(2, 3.5)                   // Set the item at a given index
newList remove("A")                     // Remove an item
newList atInsert(2, "Z")                // Insert an item at a given index
newList println                         // Let's have a look at our list

"" println

// Lists - foreach, select and map
// foreach
// 3 forms - in this example we will use the different forms to iterate over a list
testList := list("Ham", "Spam", "Jam")
testList foreach(index, value, write(index, ":", value, ", "))  // index, value, expression
"" println
testList foreach(value, value println)                          // we have removed the index
testList foreach(println)                                       // we are simple sending the expression            
"" println

// select
// 3 forms - return new lists - list(1, 3, 5)
numbersList := list(1, 2, 3, 4, 5, 6)
numbersList select(index, value, value isOdd) println
numbersList select(index, value, index isOdd) println
numbersList select(value, value isOdd) println
numbersList select(isOdd) println
// To do the same operations in-place, you can use selectInPlace() 
"" println

// map
// 3 forms - return new lists
numbersList := list(1, 2, 3, 4, 5, 6)
numbersList map(index, value, value + index) println
numbersList map(value, value*2) println
numbersList map(*2) println
// To do the same operations in-place, you can use mapInPlace() 

"" println

// MAPS (Hashes in other languages)
// Maps: Collections of key, value pairs
servitor := Map clone                       // this is how you create a map
servitor type println                       // do the type thing - Map
servitor slotNames println                  // gotta do the slot thing - empty list()
servitor atPut("type", "Mouse")             // let's try some stuff with maps    
servitor at("type") println
servitor atPut("name", "Dave")
servitor asObject println
servitor asList println
servitor keys println
servitor size println

"" println

// Ranges
// A range is a container containing a start and an end point, 
// and instructions on how to get from the start, to the end.
// Using Ranges is often convenient when creating large lists of 
// sequential data as they can be easily converted to lists, or as a replacement for the for() method.
// You can use the method on Number 'out of the box' in Io
1 to(5) foreach(value, value println)

"" println
// The methods openForAppending, openForReading, or openForUpdating are 
// used for opening files. To erase an existing file before opening a 
// new open, the remove method can be used.
// I/O – Writing Files
// writing to a plain text file format
file := File with("example.txt")
file remove // This will delete the file if it exists
file openForUpdating // Open a new file for writing
file write("This is an example of writing to a .txt file.\n")
file write("You can add more lines as needed.\n")
file close // Always remember to close the file

// writing to a .csv file
file := File with("example.csv")
file remove // This will delete the file if it exists
file openForUpdating // Open a new file for writing
// Writing the header row
file write("Name,Age,Occupation\n")
// Writing data rows
file write("Yowlie McCatface,28,Software Developer\n")
file write("Socket Bestestboi,34,Graphic Designer\n")
file close // Always remember to close the file

// reading from a txt
file := File with("example.txt") 
file openForReading // Open the file for reading
content := file readToEnd // Read the entire content of the file
file close // Always remember to close the file
// Now 'content' holds the text content of the file
// You can print it or process it as needed
write("The content of the file is:\n", content)

// reading from a csv
file := File with("example.csv") // Replace 'example.csv' with your file name
file openForReading // Open the file for reading
content := file readToEnd // Read the entire content of the file
file close // Always remember to close the file
// Split the content into lines
lines := content split("\n")
// Using 'foreach' method to iterate over lines
lines foreach(line, 
    // Split each line by comma to get individual values
    values := line split(",")
    // Using 'foreach' method to iterate over values
    values foreach(value, writeln(value))
)

"" println
"Please enter your name: " print
name := File standardInput readLine
("Hello, " .. name .. "!") println

"Please enter your name: " print
name := File standardInput readLine
write("Hello, " .. name .. "!")

name type println

"" println

// Algorithms – Euclid’s (Subtraction Method)
euclid := method(a, b,
    while(a != b, 
        if(a > b, 
            a = a - b,
            b = b - a
        )
    )
    a
)
euclid(60, 48) println // This will return 12

// Algorithms – Euclid’s (Modulo Method)
euclidMod := method(a, b,
    while(b != 0,
        tempA := b
        tempB := a % b
        a = tempA
        b = tempB
    )
    a abs
)
euclidMod(60, 48) println // This will return 12

// Algorithms – Linear Search
linearSearch := method(array, value,
    array foreach(index, element,
        if(element == value, return index)
    )
    nil
)
myArray := list(1, 2, 3, 4, 5)
linearSearch(myArray, 3) println// This will return 2

bubbleSort := method(array,
    n := array size
    for(i, 1, n - 1, 
        for(j, 0, n - i - 1,
            if(array at(j) > array at(j + 1),
                temp := array at(j)
                array atPut(j, array at(j + 1))
                array atPut(j + 1, temp)
            )
        )
    )
    array
)
myArray := list(4, 2, 6, 3, 1, 5)
bubbleSort(myArray) println        // This will return the sorted array [1, 2, 3, 4, 5, 6]

"" println
