// Day 1 experiments
// Breaking the Ice
"Hi ho, Io" print       // prints without a newline
"Hi ho, Io" println     // prints with a newline

Vehicle := Object clone     // You create new object by cloning existing ones
Vehicle println

Vehicle slotNames println                              // checking the names of all the slots in the Vehicle object
Vehicle description := "Something to take you places"  // creating and then assigning a slot using :=
Vehicle slotNames println                              // checking the names of all the slots in the Vehicle object again
Vehicle description = "Something to take you far away" // assigning something new to the existing slot using =
Vehicle description println                            // getting a value from a slot
// Vehicle nonexistingSlot = "This won't work."        // Must define slot using := not =
Vehicle type println                                   // every object supports the type slot
Object type println

// So, we know
// You make objects by cloning other objects.
// Objects are collections of slots.
// You get a slots value by sending the slot's name to the object (sending a message)


// Objects, Prototypes, and Inheritance
Car := Vehicle clone    // we create a new object called car by sending the clone message to the Vehicle prototype
Car println
Car slotNames println   // checking the names of all the slots in the Vehicle object
Car description println // lets send the slot name description to the car object to see what we get
                        // as there is not description slot in Car it forwards the 'message' to the prototype
                        // which does

ferrari := Car clone            // Notice here the name starts with a lowercase letter
ferrari println                 // Notice I get a type of Car invoked from the 'parent' type
ferrari slotNames println       // There is no type slot in the slot names (the list() is empty) - By convention types in Io begin with uppercase

Ferrari := Car clone            // If I want ferrari to be a type it has to start with uppercase
Ferrari println                 // now I get a type of Ferrari
Ferrari slotNames println       // I can see the type slot in the slot names

// So, we know
// in Io both ferrari and Ferrari are Objects
// but Ferrari is a type (because it has a type slot) while ferrari is not


// Methods
method("So, you've come for an argument." println)      // here is how you create a method in Io in a moment we will see how to use this properly
method() type println                                   // Notice it's type of Block
Car drive := method("Vroom" println)                    // We can assign the method (as it is an object) to a slot
ferrari drive                                           // If a slot has a method, invoking the slot invokes the method
ferrari getSlot("drive") println                        // We can also get the contents of a slot whether its a variable or method
ferrari getSlot("type") println                         // notice in both getSlot gives you the parents slot if one does not exist in the object
ferrari proto println                                   // sending proto to an object gives you the prototype of the object
Car proto println

Lobby println                                           // Lobby is the master namespace that contains all the named objects

// So, we know
// Every thing is an object.
// Every interaction with an object is a message.
// You don't instantiate classes; you clone other objects called Prototypes.
// Objects remember their prototypes.
// Objects have slots.
// Slots contain objects, including method objects.
// A message returns the value in a slot or invokes the method in a slot.
// If an object can't respond to a message, it sends that message to its prototype


// Lists and Maps
// A list is an orders collection of objects of any type
// List is the prototype for all lists
// Map is the prototype for key-value pairs 

toDos := list("find my car", "find Continuum Transfunctioner")   // this is how you create a list
toDos println
toDos size println                                               // we can also get the size of a list 
toDos slotNames println                                          // I could not resist checking for slots - at this point it returns the empty list()

list(1, 2, 3, 4) println                                         // let's try some more list stuff with some methods
list(1, 2, 3, 4) average println
list(1, 2, 3, 4) sum println
list(1, 2, 3, 4) at(1) println
list(1, 2, 3, 4) append(5) println
list(1, 2, 3, 4) pop println
list(1, 2, 3, 4) prepend(0) println
list(1, 2, 3, 4) isEmpty println
list() isEmpty println

elvis := Map clone                                              // this is how you create a map
elvis slotNames println                                         // gotta do the slot thing - at this point it returns the empty list()
elvis atPut("home", "Graceland")                                // let's try some stuff with maps    
elvis at("home") println
elvis atPut("style", "rock amd roll")
elvis asObject println
elvis asList println
elvis keys println
elvis size println


// true, false, nil, and singletons
(4 < 5) println
(4 <= 3) println
(true and false) println
(true and true) println
(true or true) println
(true or false) println
(4 < 5 and 6 > 7) println
(true and 6) println
(true and 0) println                // 0 is true in Io
true proto println                  // what else is true in Io?

true clone println                  // true, false and nil are all singletons
false clone println
nil clone println

Highlander := Object clone          // We can create a singleton by by redefining the clone method to return itself rather than forwarding up the tree
Highlander println
Highlander clone := Highlander
Highlander println
Highlander clone println            // Let's play around with this Highlander singleton and see what happens with the clone and type slots
fred := Highlander clone
fred println
mike := Highlander clone
mike println
(fred == mike) println              // As you can see from the result the two clones are equal

one := Object clone
two := Object clone 
(one == two) println                // Compare that with the result when an object is not a singleton

// Be very careful in Io as you can change just about any slot on any object even ones that define the object
// For example you could override the clone method on Object so that nothing can create objects anymore
// Object clone:= "hosed"