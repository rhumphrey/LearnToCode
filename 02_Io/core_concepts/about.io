// Io is a pure object-oriented programming language inspired by Smalltalk, Self, Lua, Lisp, Act1, and NewtonScript. 
// It’s known for its prototype-based object model, which eliminates the distinction between instances and classes. 
// In Io, everything is an object, and it uses dynamic typing. Programs are structured as trees of messages, similar to Lisp’s code-as-data philosophy. 
// Io also features actor-based concurrency and a small, portable virtual machine.
// The Io programming language was created by Steve Dekorte in 2002. 
// He developed Io after attempting to assist a friend with another language, which led him to explore language design and creation. 
// Steve Dekorte's work on Io has influenced the design of other languages and contributed to discussions on prototype-based object-oriented programming and dynamic languages.

// Source: Conversation with Bing, 2024-03-21
// (1) Io (programming language) - Wikipedia. https://en.wikipedia.org/wiki/Io_%28programming_language%29.
// (2) The Io Programming Language | Bushido Codes. https://www.bushido.codes/io-lang/.
// (3) Meet Val: The New Programming Language Created by a Woman. https://devstyler.io/blog/2023/07/31/meet-val-the-new-programming-language-created-by-a-woman/.

// Creating a new object and assigning values to slots
myObject := Object clone do(
    slot1 := "This is a slot"
    slot2 := 42
)

// Conditional statement and looping
if(myObject slot2 > 40, 
    "The value is greater than 40" println
)

// Looping through numbers
for(i, 1, 5, 
    i println
)

// Defining and using a method
myObject doSomething := method(
    "Doing something" println
)
myObject doSomething

myObject slot1 println
