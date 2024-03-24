// Day 1 Self-Study (Homework)
// Find:
// Some Io example problems - https://www.bushido.codes/io-lang | https://stackoverflow.com/questions/tagged/iolanguage | https://iolanguage.org/links.html
// An Io community that will answer question - https://wiki.c2.com/?IoLanguage | https://github.com/IoLanguage/io | https://en.wikipedia.org/wiki/Io_(programming_language) 
// A style guide with Io idioms - https://iolanguage.org/guide/guide.html | https://en.wikibooks.org/wiki/Io_Programming/Io_Style_Guide
//
// Answer:
// 1. Evaluate 1 + 1 then 1 + "one". Is IO strongly typed or weakly typed? Support your answer with code
// 2. Is 0 true or false? What about empty string? Is nil true or false? Support your answer with code
// 3. How can you tell what slots a prototype supports?
// 4. What is the difference between = (equals), := (colon equals), and ::= (colon colon equals)? When would you use each one?
//
// Do:
// 5. Run an Io program from a file
// 6. Execute the code in a slot given its name
// 7. Spend a little time playing with slots and prototypes. Make sure you understand how prototypes work.


// 1. Evaluate 1 + 1 then 1 + "one". Is IO strongly typed or weakly typed? Support your answer with code
// 1 + 1 evaluates to 2 because both operands are numbers.
// 1 + "one" would result in an error since Io is strongly typed, 
// meaning it does not allow operations between incompatible types without explicit conversion.
// Here’s an example to demonstrate this:
(1 + 1) println                 // This is valid and returns 2
// (1 + "one") println          // If you uncooment this it will cause a type error because "one" is a string
1 type println
"one" type println
"" println

// 2. Is 0 true or false? What about empty string? Is nil true or false? Support your answer with code
// 0 is considered true in Io.
// An empty string "" is also true.
// nil is considered false.
// Here’s how you can test it:
if(0, "true", "false") println      //  "true"
if("", "true", "false") println     //  "true"
if(nil, "true", "false") println    //  "false"
// or this would also work
(0 and true) println
("" and true) println
(nil and true) println
"" println

// 3. How can you tell what slots a prototype supports?
// You can find out what slots a prototype supports by using the slotNames method, which will return a list of slot names that the object supports.
// Here is an example:
// Create a new object as a clone of the base Object prototype
myObjectA := Object clone
// Add some slots to the object
myObjectA slotA := "This is slot A"
myObjectA slotB := "This is slot B"
myObjectA slotC := 123
myObjectA slotNames println
"" println

// 4. What is the difference between = (equals), := (colon equals), and ::= (colon colon equals)? When would you use each one?
// = is used to assign a value to an existing slot.
// := is used to create a new slot and assign a value to it.
// ::= is used to create a slot, create a setter, and assign a value.
// The setter method is automatically called whenever the slot’s value is updated, 
// allowing for additional logic to be executed during the assignment.
// Creating a new slot with :=
myObjectB := Object clone
myObjectB slotA := 10
// Updating an existing slot with =
myObjectB slotA = 20
// Creating a slot with a setter using ::=
myObjectB slotB ::= 30
myObjectB slotNames println
"" println

// 5. Run an Io program from a file
// This file :)

// 6. Execute the code in a slot given its name
// Define an object with several slots containing methods
TestObject := Object clone 
TestObject one := method("You called one" println)   
TestObject two := method("You called two" println) 
TestObject three := method("You called three" println) 
TestObject one 
TestObject two
TestObject three
"" println

// 7. Spend a little time playing with slots and prototypes. Make sure you understand how prototypes work.
Object name := "Io"                                             // Creating a new slot 'name' and setting its value to 'Io'
name println                                                    // Outputs: Io
name = "Io Language"                                            // Changing the value of the 'name' slot to 'Io Language'
name println                                                    // Outputs: Io Language
Object name println                                             // Outputs: Io
Vehicle := Object clone                                         // Creating a prototype 'Vehicle'
Vehicle description := "A means of transportation"              // Creating a and assigning a 'description' slot for Vehicle
Car := Vehicle clone                                            // Creating a new object 'Car' from the 'Vehicle' prototype
Van := Vehicle clone 
Car description = "A road vehicle, typically with four wheels"  // Setting the description slot for the 'Car' object
// Accessing a slot from the prototype vs new object
Vehicle description println                                     // Outputs: A means of transportation
Car description println                                         // Outputs: A road vehicle, typically with four wheels
Van description println                                         // Outputs: A means of transportation
Object name println                                             // Outputs: Io
name println                                                    // Outputs: Io Language
"" println