// Day 2 experiments

// Conditionals and loops
// loop("getting dizzy..." println)  // this sets up an infinite loop (Control+C to break)
// a while loop takes a condition and a message to evaluate. notice the positions of ; to concatenate messages
i := 1
while(i <= 11, i println; i = i + 1); "This one goes up to 11" println
"" println
// here is the same with a for loop
for(i, 1, 11, i println); "This one goes up to 11" println
"" println
// in this for loop we have an optional increment value
for(i, 1, 11, 2, i println); "This one goes up to 11" println
"" println
// be careful in Io where you can have an optional number of arguments as its easy to make something 'the message' for an something else by mistake 
// Messages
for(i, 1, 2, 1, i println, "extra argument")
for(i, 1, 2, i println, "extra argument")
"" println
// the if control structure is implemented as a function in the form if(condition, true code, false code)
if(true, "It is true.", "It is false.") println
if(false, "It is true.", "It is false.") println
// notice what happens if I try something like this
if(true) then("It is true") else("It is false") println
if(false) then("It is true") else("It is false") println
// in contrast to something like this
if(true) then("It is true." println) else("It is false." println)
if(false) then("It is true." println) else("It is false." println)
"" println
// in Io always ask yourself what 'message' am I sending, what is the 'receiver', and how will it handle it

// Operators
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

// Messages
// in Io code everything but comment markers and the comma between arguments are messages
// a message has three components: the sender, the target, and the arguments
// the sender sends a message to a target, the target executes the message
// the call method gives you access to the meta information about any message
// so lets experiment with that - first create a couple of objects, a target and a sender 
postOffice := Object clone
postOffice packageSender := method(call sender)
// now me will create the 'mailer' to deliver a message
mailer := Object clone
mailer deliver := method(postOffice packageSender)
// now let's have the mailer deliver the message
mailer deliver
// to clarify where we are. let's have a look at these objects
postOffice println
mailer println
// let's play around some more with the target, names and arguments
postOffice messageTarget := method(call target)
postOffice messageTarget
postOffice messageArgs := method(call message arguments)
postOffice messageName := method(call message name)
postOffice messageArgs("one", 2, : three)
postOffice messageName
// and lets have another look at the objects now and play around some more
postOffice println
mailer println
postOffice messageArgs println
result := postOffice messageArgs("one", 2, :three)
result println
"" println
// most languages pass argumants as values on stacks. Io passes the message itself and the context, then the receivers 
// evaluate the message. You can implement control structures with messages. Imagine we want to implement an unless control structure
unless := method(
    (call sender doMessage(call message argAt(0))) ifFalse(
     call sender doMessage(call message argAt(1))) ifTrue(
     call sender doMessage(call message argAt(2)))
)
unless(1 == 2, write("One is not two\n" ), write("one is two\n" ))
"" println
// doMessage is somewhat like eval in Ruby but at a lower level, eval evaluates a string as code. 
// doMessage executes an arbitrary message, it inerprets the message parameters but delays binding and execution

// Reflection
// Io gives you a set of methods to see what is going on in the slots. Let's see how we can utalise these in a method called ancestors
// first we create the ancestors methods to print the slots of an objects prototype and then call ancestors on the prototype, halting the recursion
// before all the slots are printed to save time. We also dont handle a case where an object might have more than one prototype when created
Object ancestors := method(
    prototype := self proto
    if(prototype != Object, 
        writeln("Slots of " , prototype type, "\n---------------" )
        prototype slotNames foreach(slotName, writeln(slotName))
        writeln
        prototype ancestors
    )
)
// then we create a Animal prototype
Animal := Object clone
Animal speak := method("ambiguous animal noise" println)
// then a Duck instance with a speak and walk method
Duck := Animal clone
Duck speak := method("quack" println)
Duck walk := method("waddle" println)
// we then create a disco instance using Duck as a prototype. Now suppose we want to see what is going on with respect to the slots from any previous clone
// we can use the ancestors method created previously to look at the slots of the prototype
disco := Duck clone
disco ancestors
// So, every object has a prototype, and those prototypes are objects that have slots. 
// In Io, dealing with reflection has two parts.
// In the post office example, we saw message reflection. 
// Object reflection means dealing with objects and the slots on those objects. No classes are involved, anywhere.