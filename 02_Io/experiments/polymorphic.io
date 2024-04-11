// Create a new prototype called 'Animal' by cloning the base 'Object'.
// This prototype represents the generic concept of an animal.
Animal := Object clone do(
    // Define a method 'info' that, when called, returns a string
    // indicating that the animal's type is unknown.
    info := method(
        "I am an unknown animal"
    )
)

// Create a new prototype 'Cat' by cloning the 'Animal' prototype.
// This represents a more specific type of animal.
Cat := Animal clone do(
    // Override the 'info' method to return a string specific to cats.
    info := method(
        "I am a cat"
    )
)

// Similarly, create a 'Dog' prototype by cloning 'Animal'.
Dog := Animal clone do(
    // Override the 'info' method to return a string specific to dogs.
    info := method(
        "I am a dog"
    )
)

// Create a list of animal instances by cloning the 'Cat', 'Dog', and 'Animal' prototypes.
// This list represents a collection of pets.
pet := list(Cat clone, Cat clone, Dog clone, Dog clone, Animal clone)

// Iterate over the 'pet' list and print the information of each animal.
// The 'index' and 'value' represent the position and the object in the list, respectively.
pet foreach(index, value, value info println)

// This iteration will print the 'info' slot on each element.
pet foreach(info println)

// This iteration will print the string representation of each object in the list.
pet foreach(println)

// This iteration will print the type of each object in the list.
pet foreach(type println)