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


// Starting experiments
heading("Start")
"Hello world!" println
println
someObject := Object clone  // First create a clone of Object
"" println

// Inspecting Objects
heading("Inspecting Objects")
someObject println
someObject slotNames println
Object slotNames sort println
slotSummary println
Protos println
Protos Addons println
Protos Core println
"" println

// doFile and doString
heading("doFile and doString")
doFile("hello.io")
someObject doFile("hello.io") println
doString("1+1") println
someObject doString("1 + 1") println
"" println

// Command Line Arguments and launchPath
heading("Command Line Arguments and launchPath")
System args foreach(k, v, write("'", v, "'\n"))
System launchPath println
"" println

// Expressions
heading("Expressions")
"""Io has no keywords or statements. Everything is an expression composed entirely of messages, each of which is a runtime accessible object. The informal BNF description:

exp        ::= { message | terminator }
message    ::= symbol [arguments]
arguments  ::= "(" [exp [ { "," exp } ]] ")"
symbol     ::= identifier | number | string
terminator ::= "\n" | ";"

For performance reasons, String and Number literal messages have their results cached in their message objects.""" println
"" println

// Messages
heading("Messages")
for(i, 1, 10, i println)
b := 1                      // Experiment with changing b to see what happens
c := 2
d := 4
a := if(b == 0, c + 1, d)
a println
// Create a Map where each key is a person's name and the value is another Map with their attributes
people := Map clone
// Add people to the 'people' Map
people atPut("Alice", Map clone atPut("age", 25) atPut("name", "Alice"))
people atPut("Bob", Map clone atPut("age", 32) atPut("name", "Bob"))
people atPut("Charlie", Map clone atPut("age", 28) atPut("name", "Charlie"))
// Select people younger than 30
youngPeople := people select(key, value, value at("age") < 30)
// Get the names of the selected people
names := youngPeople map(key, value, value at("name"))
names println
// There is also some syntax sugar for operators (including assignment), which are handled 
// by an Io macro executed on the expression after it is compiled into a message tree.
Account := Object clone
Account balance := 0
Account deposit := method(amount,
    balance = balance + amount
)
account := Account clone
account deposit(10.00)
account balance println
"" println

// Operators
heading("Operators")
1 + 2 println
(1 + 2) println // remember to parenthesise so that the println method message is passed to the ressult of + and not just the 2
(1 +(2 * 4)) println
(1 + 2 * 3 + 4) println
(1 + (2 * 3) + 4) println
(1 +(2 *(3)) +(4)) println
"" println
// OperatorTable
// Operators can also be added or removed, or have their precedence changed by using the 
// global OperatorTable object
OperatorTable println // before change
OperatorTable addOperator("!!", 3) // adds the !! operator with a precedence of 3
// Note that this change will not affect the source file in which the OperatorTable 
// is modified as the full file is parsed before it is evaluated.
OperatorTable println // after change
"" println

// Assignment
heading("Assignment")
a ::= 1         // Creates slot, creates setter, assigns value
a := 1          // Creates slot, assigns value
a = 1           // Assigns value to slot if it exists, otherwise raises exception
Lobby slotNames println
a println
"" println


// Numbers
heading("Numbers")
"""The following are examples of valid number formats:
123
123.456
0.456
.456
123e-4
123e4
123.456e-7
123.456e2

Hex numbers are also supported (in any casing):
0x0
0x0F
0XeE """ println
"" println

// Strings
heading("Strings")
escapedString := "this is a \"test\".\nThis is only a test."
escapedString println
multilineString := """this is a "test".
This is only a test."""
multilineString println
"" println

// Comments
heading("Comments")
"""Comments of the //, /**/ and # style are supported. Examples:
a := b // add a comment to a line

/* comment out a group
a := 1
b := 2
*/

The # style is useful for unix scripts:
#!/usr/local/bin/io """ println
"" println

// Objects
heading("Objects")
"""Io's guiding design principle is simplicity and power through conceptual unification.
concept         unifies
scopable        blocks	functions, methods, closures
prototypes      objects, classes, namespaces, locals
messages        operators, calls, assigns, var access """ println
"" println

// Prototypes
heading("Prototypes")
"""In Io, everything is an object (including the locals storage of a block and the namespace itself) 
and all actions are messages (including assignment). Objects are composed of a list of key/value 
pairs called slots, and an internal list of objects from which they inherit called protos. 
A slot's key is a symbol (a unique immutable sequence) and its value can be any type of object. 

New objects are made by cloning existing ones. A clone is an empty object that has the parent 
in its list of protos. When an object is cloned, the new instance's init slot will be activated 
which gives the object a chance to initialize itself. Like NewtonScript, slots in Io are create-on-write. """ println
"" println
Person := Object clone
me := Person clone
myDog := Object clone
myDog name := "rover"
myDog sit := method("I'm sitting\n" print)
Object println
Person println
me println
myDog println
myDog name println
myDog sit
Object protos println
"" println

// Inheritance
heading("Inheritance")
Dog := Object clone
Dog println
Dog color := "red"
Dog println

// Methods
heading("Methods")
twoPlusTwo := method((2 + 2) println)
twoPlusTwo
Dog := Object clone
Dog bark := method("woof!" println)
Dog bark
x := 1
y := 2
add := method(a, b, a + b)
add(x,y) println
"""The general form for a method is:

method(<arg name 0>, <arg name 1>, ..., <do message>) """ println
"" println

// Blocks
heading("Blocks")
"""A block is the same as a method except it is lexically scoped. 
That is, variable lookups continue in the context of where the block 
was created instead of the target of the message which activated the block. 
A block can be created using the Object method block(). 
Example of creating a block:
b := block(a, a + b)

Both methods and blocks create an object to hold their locals when they are called. 
The difference is what the "proto" and "self" slots of that locals object are set to. 
In a method, those slots are set to the target of the message. 
In a block, they're set to the locals object where the block was created. 
So a failed variable lookup in a block's locals continue in the locals where it was created. 
And a failed variable lookup in a method's locals continue in the object to which the 
message that activated it was sent.
call and self slots
When a locals object is created, its self slot is set (to the target of the message, 
in the case of a method, or to the creation context, in the case of a block) 
and its call slot is set to a Call object that can be used to access information about the block activation:
slot                returns
call sender         locals object of caller
call message        message used to call this method/block
call activated      the activated method/block
call slotContext    context in which slot was found
call target         current object """ println
"" println
// Variable Arguments - experiment with changing values for a and b
a = 1
b = 1
myif := method(
    (call sender doMessage(call message argAt(0))) ifTrue( 
    call sender doMessage(call message argAt(1))) ifFalse( 
    call sender doMessage(call message argAt(2)))
)
myif(a == b, write("true\n"), write("false\n"))
// The doMessage() method evaluates the argument in the context of the receiver. 
// A shorter way to express this is to use the evalArgAt() method on the call object:
myif := method(
    call evalArgAt(0) ifTrue(
    call evalArgAt(1)) ifFalse( 
    call evalArgAt(2))
)
myif(a == b, write("true\n"), write("false\n"))
"" println

// Forward
heading("Forward")
// If an object doesn't respond to a message, it will invoke its "forward" method if it has one. 
// Here's an example of how to print the information related lookup that failed:
MyObject := Object clone
MyObject forward := method(
    write("sender = ", call sender, "\n")
    write("message name = ", call message name, "\n")
    args := call message argsEvaluatedIn(call sender)
    args foreach(i, v, write("arg", i, " = ", v, "\n") )
)
MyObject forward println
"" println

// Resend
heading("Resend")
// Sends the current message to the receiver's protos with self as the context. Example:
A := Object clone
A m := method(write("in A\n"))
B := A clone
B m := method(write("in B\n"); resend)
B m
"" println

// Super
heading("Super")
// Sometimes it's necessary to send a message directly to a proto.
Dog := Object clone
Dog bark := method(writeln("woof!"))
fido := Dog clone
fido bark := method(
    writeln("ruff!")
    super(bark)
)
fido bark println
"" println

// Introspection
heading("Introspection")
// slotNames - returns a list of the names of an object's slots
Dog slotNames println
// protos - returns a list of the objects which an object inherits from
Dog protos println
// getSlot - get the value of a block in a slot without activating it
dogBarkMethod := Dog getSlot("bark")
dogBarkMethod println
// if you then want to use the dogBarkMethod without activating it, you'll need to use the getSlot method:
otherObject := getSlot("dogBarkMethod")
otherObject println
// code -  convenience method, which returns a string representation of the code of the method in a normalized form.
method(a, a * 2) code println
"" println

// Control Flow
heading("Control Flow")
"""true, false and nil - There are singletons for true, false and nil. 
nil is typically used to indicate an unset or missing value 

The comparison methods:
==, !=, >=, <=, >, < 
return either the true or false. 
The compare() method is used to implement the comparison methods and returns -1, 0 or 1 
which mean less-than, equal-to or greater-than, respectively. 
The if() method can be used in the form:
         if(<condition>, <do message>, <else do message>)

The else argument is optional. 
The condition is considered false if the condition expression evaluates to false or nil, 
and true otherwise.
Conditions can also be used in this form:
        if(y < 10) then(x := y) else(x := 2)
elseif() is supported:
        if(y < 10) then(x := y) elseif(y == 11) then(x := 0) else(x := 2) """ println
"" println
// if, then, else
a = 10
if(a == 10, "a is 10" println)
y = 11
if(y < 10, x := y, x := 0) println
x := if(y < 10, y, 0) println  // is the same as the previous code
if(y < 10) then(x := y) else(x := 2)
x println
if(y < 10) then(x := y) elseif(y == 11) then(x := 0) else(x := 2)
x println
// ifTrue, ifFalse - Smalltalk style
(y < 10) ifTrue(x := y) ifFalse(x := 2) 
x print
// loop
l := 0
loop(
    if(l > 5, 
        break, 
    l println  
    l = l + 1
    )
)
// repeat
3 repeat("x" print)
"" println
// while - while(<condition>, <do message>)
a := 1
while(a < 10, 
    a print
    a = a + 1
)
// for - for(<counter>, <start>, <end>, <optional step>, <do message>)
// The start and end messages are only evaluated once, when the loop starts. Example:
for(a, 0, 10, 
    a println
)
for(x, 0, 10, 3, x println)     // with a step
for(a, 10, 0, -1, a println)    // To reverse the order of the loop, add a negative step
// break, continue - loop, repeat, while and for support the break and continue methods
for(i, 1, 10, 
    if(i == 3, continue)
    if(i == 7, break)
    i print
)
"" println
// return - Any part of a block can return immediately using the return method
test := method(123 print; return "abc"; 456 print)
test
"" println
test println
"" println

// Importing
heading("Importing")
"""The Importer proto implements Io's built-in auto importer feature. 
If you put each of your proto's in their own file, and give the file the same name with and ".io" extension, 
the Importer will automatically import that file when the proto is first referenced. 
The Importer's default search path is the current working directory, 
but can add search paths using its addSearchPath() method.""" println
"" println

// Concurrency
heading("Concurrency")
"""Io uses coroutines (user level cooperative threads), instead of preemptive OS level threads to implement concurrency. 
This avoids the substantial costs (memory, system calls, locking, caching issues, etc) associated with native threads 
and allows Io to support a high level of concurrency. 
Io's coroutines stacks are much smaller than native thread stacks but can automatically increase in size (by chaining coroutines) 
as needed. Io's networking libraries leverage coroutines by automatically pausing the calling coroutine (and scheduling another) 
when waiting on read/write operations, and resuming them when the result (which may be an error or timeout exception) 
of the read/write is available.

The Scheduler object is responsible for resuming coroutines that are yielding. 
The current scheduling system uses a simple first-in-first-out policy with no priorities.

An actor is an object with its own thread (in our case, its own coroutine) which it uses to process its queue of 
asynchronous messages. Any object in Io can be sent an asynchronous message by using the asyncSend() or futureSend() messages.""" println
"" println
// actors
obj1 := Object clone
obj1 test := method(for(n, 1, 3, n print; yield))
obj2 := obj1 clone
obj1 asyncSend(test); obj2 asyncSend(test)
while(Scheduler yieldingCoros size > 1, yield)
"" println
"""The command line will attempt to print the result of expressions evaluated in it, so if the result is a Future, 
it will attempt to print it and this will wait on the result of Future.""" println
q := method(wait(1))
futureSend(q) println
"""To avoid this, just make sure the Future isn't the result.""" println
(futureSend(q); nil) println
"" println
