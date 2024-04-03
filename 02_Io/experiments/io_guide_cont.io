// heading method
heading := method(title, do(
    (title size) repeat("-" print)
    "" println
    title println
    (title size) repeat("-" print)
    "" println
    )
)
// Working through and experimenting with the code concepts in the Io guide - https://iolanguage.org/guide/guide.html

// Exceptions
heading("Exceptions")
"""An exception can be raised by calling raise() on an exception proto.
    Exception raise("generic foo exception")
    
To catch an exception, the try() method of the Object proto is used. 
try() will catch any exceptions that occur within it and return the 
caught exception or nil if no exception is caught.
    e := try(<doMessage>)
    
To catch a particular exception, the Exception catch() method can be used.
    e := try(
        // ...
    ) 

    e catch(Exception,
        writeln(e coroutine backTraceString)
    ) 
    The first argument to catch indicates which types of exceptions will be caught. 
    catch() returns the exception if it doesn't match and nil if it does. 
    
    To re-raise an exception caught by try(), use the pass method. 
    This is useful to pass the exception up to the next outer exception handler, 
    usually after all catches failed to match the type of the current exception:
    e := try(
        // ...
    ) 

    e catch(Error,
        // ...
    ) catch(Exception,
        // ...
    ) pass 
    
    Custom exception types can be implemented by simply cloning an existing 
    Exception type:
        MyErrorType := Error clone""" println
"" println

// Primitives
heading("Primitives")
obj := Object clone
obj a := 42
// The ? Operator
if(obj getSlot("a"), obj a) println     // This 
obj ?a println                          // can be simplified using ?
// List - indexes begin at 0
a := List clone                         // Create an empty list
a := list(33, "a")                      // Create a list using the list method
a println
a append("b")                           // Append an item
a append("Ernie")                       // Append an item
a println
a size println                          // Get the list size
a at(1) println    
a atPut(2, "Bert")                      // Set at a given index
a println
// a atPut(6, "Fred") // uncomment this to see the error
a println
a remove("Bert")                        // Remove a given item
a println
a atInsert(2, "Fred")                   // Insert at a given index
a println
// for each examples - 3 forms
a := list(65, 21, 122)
a foreach(index, value, write(index, ":", value, ", "))     // index, value. expression
"" println
a foreach(value, value println)                             // removing index
a foreach(println)                                          // removing value
// map and select
// Io's map and select (known as filter in some other languages) 
// methods allow arbitrary expressions as the map/select predicates.
// The map and select methods return new lists. 
// To do the same operations in-place, you can use selectInPlace() and mapInPlace() methods.
numbers := list(1, 2, 3, 4, 5, 6)
numbers select(isOdd) println
numbers select(x, x isOdd) println
numbers select(i, x, x isOdd) println
numbers map(x, x*2) println
numbers map(i, x, x+i) println
numbers map(*3) println
// Sequence
// In Io, an immutable Sequence is called a Symbol and a mutable Sequence is the equivalent of a Buffer or String. 
// Literal strings(ones that appear in source code surrounded by quotes) are Symbols. Mutable operations cannot be 
// performed on Symbols, but one can make mutable copy of a Symbol calling its asMutable method and then 
// perform the mutation operations on the copy. Common string operations.
"firefly" size println                      // Getting the length of a string
"apples" containsSeq("ppl") println         // Checking if a string contains a substring
"Kavi" at(1) println                        // Getting the character (byte) at position N
"Kirikuro" exSlice(0, 2) println            // slicing
"Kirikuro" exSlice(-2)    println           // NOT: exSlice(-2, 0)!
"Kirikuro" exSlice(0, -2) println
"  abc  " asMutable strip println           // striping whitespace
"  abc  " asMutable lstrip println
"  abc  " asMutable rstrip println
"Kavi" asUppercase println                  // case changing
"Kavi" asLowercase println
"the quick brown fox" split println         // split string -> a list
"a few good men" split("e") println         // split by a character -> a list
"13" asNumber println                       // converting
"a13" asNumber println
name := "Ted"
"My name is #{name}" interpolate println    // interpolation
// Ranges
"" println
"""Note: Each object that can be used in Ranges needs to implement a "nextInSequence" method which takes 
a single optional argument (the number of items to skip in the sequence of objects), 
and return the next item after that skip value. The default skip value is 1. The skip value of 0 is undefined. 
An example:
    Number nextInSequence := method(skipVal,
        if(skipVal isNil, skipVal = 1)
        self + skipVal
    ) 
With this method on Number (it's actually already there in the standard libraries), 
you can then use Numbers in Ranges.""" println
"""Note: I am seeing problems generally using Ranges in the language 
    1 to(5) foreach(value, value println)
The above does not work as implied, (you get an error regarding to() in the Number object). 
I did some introspection of the slots on Number and its not present but then starts working 
in cases where I have previously created a Range object and then the to() method 
suddenly appears in Numbers slots when I perform another introspection""" println
"" println
// File - The methods openForAppending, openForReading, or openForUpdating are used for opening files. 
// To erase an existing file before opening a new open, the remove method can be used.
file := File with("test.txt")
file remove
file openForUpdating
file write("Hello world!")
file close
// Directory
dir := Directory with("C:/Users/admin/OneDrive/Programming/_learntocode_/02_Io/experiments/")
files := dir files
files println
items := Directory items
items println
items at(4) name println
root := Directory clone setPath("c:/")
root fileNames println
Directory clone setPath("q:/") exists println
Directory slotNames println
Directory currentWorkingDirectory println
// Dates 
newDate := Date clone       // Creating a new date instance
newDate type println
newDate println
newDate now                 // Setting it to the current date/time
newDate println
Date now asNumber println
Date now asNumber println
d := Date now               // Creating and setting an instance
d type println
d println
d year println
d month println
d day println
d hour println
d minute println
d second println
Date cpuSecondsToRun(100000 repeat(1+1)) println  // Timing some code execution
// Networking
// I had a several problems getting the code in this part of the guide to run
// https://iolanguage.org/guide/guide.html#Primitives-Networking
// I need to read up om how to load the appropriate Networking add-ons
// as it is not recognising some of the objects used in the code
// XML - the same problem as above
// Vector -  built on Io's Sequence primitive
Vector := Sequence clone setItemType("float32")
iters := 1000
size := 1024
ops := iters * size
v1 := Vector clone setSize(size) rangeFill
v2 := Vector clone setSize(size) rangeFill
dt := Date secondsToRun(
    iters repeat(v1 *= v2)
)
writeln((ops/(dt*1000000000)) asString(1, 3), " GFLOPS")
"" println

// Unicode - https://iolanguage.org/guide/guide.html#Unicode
heading("Unicode")
"https://iolanguage.org/guide/guide.html#Unicode" println
"" println


// Embedding - https://iolanguage.org/guide/guide.html#Embedding
heading("Embedding")
"https://iolanguage.org/guide/guide.html#Embedding" println
"" println