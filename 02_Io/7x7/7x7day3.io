// Day 3 experiments

// Domain-Specific Languages
// Changing the syntax of Io
//
// Add a custom assignment operator ":" to the OperatorTable with the action "atPutNumber"
OperatorTable addAssignOperator(":" , "atPutNumber" )
// Define a method associated with curly brackets that creates a new Map and populates it
curlyBrackets := method(
    r := Map clone                                  // Clone a new Map object
    call message arguments foreach(arg,             // Iterate over each argument passed to the curly brackets
        r doMessage(arg)                            // Execute the message on the Map object
    )
    r
)
// Define a method "atPutNumber" for the Map class to assign values to keys without quotes
Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix(" \"" ),    // Evaluate the first argument, remove quotes, and use it as the key
        call evalArgAt(1))                                                      // Evaluate the second argument and use it as the value
)
// Read the contents of "phonebook.txt" into a string
s := File with("phonebook.txt" ) openForReading contents
// Evaluate the string as Io code, which populates the phoneNumbers Map using the curlyBrackets method
phoneNumbers := doString(s)
// Print the keys (names) and values (numbers) of the phoneNumbers Map
phoneNumbers keys println
phoneNumbers values println
"" println
//
// Additional Notes:
// In the above code the phonebook.txt file contains a Map represented in Io’s syntax, 
// where each key-value pair represents a person’s name and their phone number, respectively. 
// The custom atPutNumber method allows the code to read the keys without the need for quotes around them. 
// When the file is read and evaluated, the phoneNumbers Map will contain the names as keys and the corresponding phone numbers as values, 
// which are then printed out.
// This is also summarzed in the '7 in 7 weeks' book, as follows:
// The doString message evaluates our phone book as code, 
// File is a prototype for working with files, with specifies a filename and returns a file object, 
// openForReading opens that file and returns the file object, 
// contents returns the contents of that file. 
// Taken together, this code will read the phone book and evaluate it as code.


// Io’s method_missing
// Changing the behaviour of Io
// Review of the flow of control in Io - The behaviour for what happens in a given message (baked into Object) - The 'mechanics of inheritance'
// 1. Compute the arguments, inside out. These are just messages.
// 2. Get the name, target, and sender of the message.
// 3. Try to read the slot with the name of the message on the target.
// 4. If the slot exists, return the data or invoke the method inside.
// 5. If the slot doesn’t exist, forward the message to the prototype.
// 
// However it is possible, in Io, to mess with the basic mechanics of inheritence 
// 
// Clone the Object prototype to create a new Builder object
Builder := Object clone
// Define a 'forward' method on the Builder object
// This method will be called when a message is sent to Builder that it does not understand
Builder forward := method(
    // Write the opening tag with the name of the unknown message
    writeln("<" , call message name, ">" )
    // Iterate over the arguments of the unknown message
    call message arguments foreach(
        arg,
        // Send each argument as a message to self (Builder) and store the result in 'content'
        content := self doMessage(arg);
        // If the result 'content' is a Sequence (string), write it
        if(content type == "Sequence" , writeln(content)))
    // Write the closing tag with the name of the unknown message
    writeln("</" , call message name, ">" ))
// Use the Builder to construct an unordered list (ul) with list items (li)
// Each 'li' message is forwarded to the 'forward' method, which handles it
Builder ul(
    li("Io" ),
    li("Lua" ),
    li("JavaScript" ))
"" println
//
// Aditional Notes:
// the code above defines a simple HTML builder that can generate HTML tags dynamically using Io’s message forwarding capability. 
// When you send a message to the Builder that it doesn’t understand (like ul or li), it uses the forward method to handle the message. 
// It prints the opening tag, processes the arguments as content between the tags, and then prints the closing tag. 
// The result of running this code would be the generation of an HTML unordered list with three items.
//


// Concurrency - Coroutines
// A coroutine provides a way to voluntarily suspend and resume execution of a process.
//
vizzini := Object clone                                 // Clone the Object prototype to create a new object named 'vizzini'
vizzini talk := method(                                 // Define a 'talk' method on the 'vizzini' object
    "Fezzik, are there rocks ahead?" println            // Print the line "Fezzik, are there rocks ahead?" to the console
    yield                                               // Pause the coroutine, allowing other coroutines to run
    "No more rhymes now, I mean it." println            // Print the line "No more rhymes now, I mean it." to the console
    yield)                                              // Pause the coroutine again
fezzik := Object clone                                  // Clone the Object prototype to create a new object named 'fezzik'
fezzik rhyme := method(                                 // Define a 'rhyme' method on the 'fezzik' object
    yield                                               // Pause the coroutine, waiting for the 'talk' method to yield
    "If there are, we'll all be dead." println          // Print the line "If there are, we'll all be dead." to the console
    yield                                               // Pause the coroutine again
    "Anybody want a peanut?" println)                   // Print the line "Anybody want a peanut?" to the console
// The remaining Io code for this example starts the coroutines you need to uncomment the 2 lines marked to see it in action if you do 
// it will exit the code with the message "Scheduler: nothing left to resume so we are exiting" along with some exit information
//
// Start the 'talk' method on 'vizzini' and the 'rhyme' method on 'fezzik' as coroutines
// vizzini @@talk; fezzik @@rhyme                       // NOTE: uncomment the preceding code to see the example of Coroutines work
// Pause the current coroutine, which is the main program 
// Coroutine currentCoroutine pause                     // NOTE: uncomment the preceding code to see the example of Coroutines work
"" println
//
// Aditional Notes:
// This code snippet demonstrates the use of coroutines in Io for concurrent execution. 
// The vizzini and fezzik objects each have a method that prints lines to the console and yields control back to allow the other coroutine to run. 
// The @@ operator is used to start the methods as coroutines. 
// The Coroutine currentCoroutine pause line pauses the main program, allowing the coroutines to run until they are complete. 
// The dialogue is a reference to the characters Vizzini and Fezzik from the movie “The Princess Bride.”
// 
// A coroutine in Io, similar to other programming languages, is a component used for concurrency. 
// It’s a type of function that can be paused and resumed, allowing for multiple tasks to be performed without blocking the execution of the program. 
// Coroutines are particularly useful for handling asynchronous operations, such as I/O tasks, without the overhead of creating new threads.
//
// In Io, coroutines are represented by the Coroutine object and can be created using the @ operator. 
// They allow the program to multitask by yielding control with the yield keyword, which pauses the coroutine and allows other coroutines to run. 
// When a coroutine is resumed, it picks up where it left off. This makes coroutines a powerful tool for writing efficient and responsive applications.
//
// Here’s a high-level overview of how coroutines work in Io:
// Creation: A coroutine is created from a regular Io method using the @ operator.
// Yielding: The yield keyword is used within the coroutine to pause its execution and allow other coroutines to run.
// Resuming: A paused coroutine can be resumed, continuing its execution from the point where it yielded.
// Coroutines in Io provide a way to perform non-blocking operations and are essential for tasks that require concurrency or asynchronous processing. 
// They help to keep the program responsive by allowing other operations to proceed while waiting for an operation to complete.
//


// Concurrency - Actors
// Sending an asynchronous message to any object makes it an actor.
//
slower := Object clone                                  // Create a new object 'slower' by cloning the base Object prototype
faster := Object clone                                  // Create another new object 'faster' by cloning the base Object prototype
slower start := method(wait(2); writeln("slowly"))      // Define a 'start' method on the 'slower' object that waits for 2 seconds then prints "slowly"
faster start := method(wait(1); writeln("quickly"))     // Define a 'start' method on the 'faster' object that waits for 1 second then prints "quickly"
slower start; faster start                              // Call the 'start' method on both 'slower' and 'faster' objects sequentially
slower @@start; faster @@start; wait(3)                 // Start the 'start' methods on both 'slower' and 'faster' objects as coroutines
//                                                      // The main program waits for 3 seconds before ending
"" println
//
// Aditional Notes:
// In this code, two objects are created, each with a start method that includes a wait statement. 
// The wait function pauses the program for the specified number of seconds. 
// When the start methods are called sequentially, they will execute one after the other, resulting in “slowly” being printed after 2 seconds, 
// followed by “quickly” after another second.
// However, when the start methods are invoked as coroutines (using @@), they run concurrently. 
// This means “quickly” will be printed after 1 second, and “slowly” will be printed after 2 seconds, regardless of their order in the code. 
// The wait(3) at the end ensures that the main program waits long enough for both coroutines to finish executing before it terminates.
//


// Concurrency - Futures - NOTE: Needs more work Problems with URL - *****
// 
// Create a 'futureResult' object that starts fetching the contents of the specified URL in the background
futureResult := URL with("http://google.com/") @fetch
// Print a message to the console indicating that the program will continue executing while the fetch is in progress
writeln("Do something immediately while fetch goes on in background...")
// Print a message to the console indicating that the next operation will wait for the 'futureResult' to be ready
writeln("This will block until the result is available.")
// Once the 'futureResult' is ready, print the size of the fetched content in bytes to the console
writeln("fetched ", futureResult size, " bytes")
//
// Aditional Notes:
// In this code, @fetch is used to start the asynchronous operation of fetching the contents of the given URL. 
// The futureResult object represents the eventual result of this operation. 
// While the fetch is happening in the background, the program can perform other tasks. 
// However, when it needs the result from futureResult, it will block and wait for the fetch to complete before proceeding. 
// Once the fetch is complete, the size of the fetched content is printed to the console. 
// This demonstrates how Io can handle asynchronous operations without blocking the main program’s execution.
//
// In Io, futures are a concurrency feature that allows for non-blocking asynchronous operations. 
// A future represents a value that may not yet be computed but will be available at some point. 
// When you create a future in Io, the operation is sent off to be processed, and the future object immediately returns. 
// The rest of your code continues to execute without waiting for the future’s operation to complete.
// Here’s a high-level explanation of how futures work in Io:
// Creation: You create a future by sending an asynchronous message to an object. In Io, this is done using the @ operator.
// Non-blocking: The future is executed in the background, allowing the main program to continue running without waiting for the result.
// Result Retrieval: Once the future has completed its operation, you can retrieve the result. 
// If you try to access the result before it’s ready, the program will block until the result is available.
// 
// Futures are useful for tasks like fetching a web page, reading a file, or any other operation that would normally block the program’s execution. 
// By using futures, you can keep your application responsive and efficient.
//
