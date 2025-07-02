package main

import (
	"bufio"   // For reading line-by-line input
	"errors"  // For custom errors, used for explicit error handling
	"fmt"     // For formatted I/O, essential for printing
	"os"      // For interacting with the operating system (like stdin)
	"strconv" // For converting strings to integers
	"strings" // For string manipulation (like trimming whitespace)
	"time"    // For time-related functions, especially useful with goroutines
)

// --- Global Variables and Type Definitions ---
// These are defined at the package level to be accessible by all functions within the 'main' package.

// scanner is a global variable to reuse the bufio.Scanner for reading input.
// This is more efficient than creating a new scanner for each input operation.
var scanner *bufio.Scanner

// For Experiment 10: init() function.
// This variable's state will be set by the init() function, demonstrating its execution timing.
var initRan bool

// For Experiment 3 & 4: Structs and Interfaces defined at package level.
// Defining them here prevents re-declaration errors when used in multiple experiments.

// Circle represents a geometric circle with a given radius.
type Circle struct {
	Radius float64
}

// Area is a method associated with the Circle type.
// It calculates and returns the area of the circle.
// (c Circle) is the receiver argument, making Area a method of Circle.
func (c Circle) Area() float64 {
	return 3.14159 * c.Radius * c.Radius // Using a common approximation for Pi
}

// Rectangle represents a geometric rectangle with given width and height.
type Rectangle struct {
	Width, Height float64
}

// Area is a method associated with the Rectangle type.
// It calculates and returns the area of the rectangle.
// (r Rectangle) is the receiver argument, making Area a method of Rectangle.
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

// Shape is an interface that defines a contract: any type that implements
// an `Area() float64` method implicitly satisfies the Shape interface.
// This is Go's way of achieving polymorphism.
type Shape interface {
	Area() float64
}

// printArea is a helper function for Experiment 4.
// It accepts any type that satisfies the 'Shape' interface, demonstrating polymorphism.
func printArea(s Shape) {
	fmt.Printf("The area is: %.2f\n", s.Area())
}

// init() function for Experiment 10.
// This special function is automatically executed once when the package is initialized,
// before the main() function runs. It's ideal for one-time setup or verification.
func init() {
	fmt.Println("\n[INFO: init() function called. This runs once before main().]")
	initRan = true // Set the flag to indicate init() has run
}

// --- Main Program Logic ---

// main is the entry point of the application.
func main() {
	// Initialize the scanner to read from standard input.
	scanner = bufio.NewScanner(os.Stdin)

	// The main program loop. It continues indefinitely until the user chooses to exit.
	for {
		displayMenu() // Show the menu options to the user.

		// Prompt for and read user input.
		inputStr := readInput("Enter your choice (0-10): ")

		// Attempt to convert the input string to an integer.
		choice, err := strconv.Atoi(inputStr)
		if err != nil {
			// If conversion fails (e.g., non-numeric input), print an error.
			fmt.Println("Error: Invalid input. Please enter a number.")
			pressEnterToContinue() // Pause to allow user to see the error.
			continue               // Skip to the next iteration of the loop (redisplay menu).
		}

		// Dispatch based on the user's valid integer choice.
		switch choice {
		case 0:
			// Exit option.
			fmt.Println("\nExiting program. Goodbye!")
			return // Terminates the main function, thus ending the program.
		case 1:
			experiment1()
		case 2:
			experiment2()
		case 3:
			experiment3()
		case 4:
			experiment4()
		case 5:
			experiment5()
		case 6:
			experiment6()
		case 7:
			experiment7()
		case 8:
			experiment8()
		case 9:
			experiment9()
		case 10:
			experiment10()
		default:
			// Handle choices outside the valid range (1-10, excluding 0 for exit).
			fmt.Println("Error: Invalid choice. Please enter a number between 0 and 10.")
		}

		// After an experiment runs or an invalid choice is made, pause before redisplaying the menu.
		pressEnterToContinue()
	}
}

// displayMenu prints the main menu options to the console.
func displayMenu() {
	fmt.Println("\n--- Interactive Experiment Dispatcher ---")
	fmt.Println("Select an option to run an experiment:")
	fmt.Println("---------------------------------------")
	menuItems := []string{
		"1. Run Experiment 1: Multiple Return Values (Error Handling)",
		"2. Run Experiment 2: Goroutines and Channels (Concurrency)",
		"3. Run Experiment 3: Structs and Methods (Object-Oriented Go)",
		"4. Run Experiment 4: Interfaces (Polymorphism)",
		"5. Run Experiment 5: Defer Statement (Resource Management)",
		"6. Run Experiment 6: Pointers (Direct Memory Access)",
		"7. Run Experiment 7: Slices (Dynamic Arrays)",
		"8. Run Experiment 8: Maps (Key-Value Pairs)",
		"9. Run Experiment 9: Variadic Functions (Flexible Arguments)",
		"10. Run Experiment 10: init() Function (Package Initialization)",
	}
	for _, item := range menuItems {
		fmt.Println(item)
	}
	fmt.Println(" 0. Exit Program")
	fmt.Println("---------------------------------------")
}

// readInput is a helper function to prompt the user and read a line of input.
// It trims leading/trailing whitespace from the input.
func readInput(prompt string) string {
	fmt.Print(prompt)
	scanner.Scan()
	return strings.TrimSpace(scanner.Text())
}

// pressEnterToContinue pauses program execution until the user presses Enter.
func pressEnterToContinue() {
	fmt.Println("\nPress Enter to continue...")
	scanner.Scan()
}

// --- Experiment Functions ---

// experiment1 demonstrates Multiple Return Values and Idiomatic Error Handling.
// Go functions commonly return multiple values. The convention for functions
// that can fail is to return a result and an 'error' type as the last return value.
// Callers are expected to explicitly check for errors, often with `if err != nil`.
func experiment1() {
	fmt.Println("\n--- Running Experiment 1: Multiple Return Values (Error Handling) ---")

	// Successful division: `err` will be nil
	result, err := divide(10, 2)
	if err != nil {
		// This block is skipped if there's no error
		fmt.Printf("Error during division: %v\n", err)
	} else {
		fmt.Printf("10 / 2 = %.2f\n", result) // Format to 2 decimal places for clarity
	}

	// Division by zero: `err` will contain an error object
	result, err = divide(10, 0)
	if err != nil {
		// This block executes because an error was returned
		fmt.Printf("Error during division: %v\n", err)
	} else {
		fmt.Printf("10 / 0 = %.2f\n", result) // This block is skipped
	}

	fmt.Println("\nPurpose:")
	fmt.Println("- Go encourages explicit error handling rather than exceptions.")
	fmt.Println("- Functions return errors directly, forcing the caller to address them.")
	fmt.Println("- This makes the flow of control clear and predictable, reducing hidden bugs.")
	fmt.Println("--- Experiment 1 Finished ---")
}

// divide is a helper function for Experiment 1.
// It returns a float64 (the result of division) and an error.
// If division by zero occurs, it returns 0 and a new error. Otherwise, it returns the result and nil.
func divide(a, b float64) (float64, error) {
	if b == 0 {
		// errors.New creates a simple error message.
		return 0, errors.New("cannot divide by zero")
	}
	return a / b, nil // 'nil' indicates no error
}

// experiment2 demonstrates Goroutines and Channels for Concurrency.
// Go's concurrency model is based on CSP (Communicating Sequential Processes).
// Goroutines are lightweight, independently executing functions.
// Channels are the primary way goroutines communicate, providing safe data exchange.
func experiment2() {
	fmt.Println("\n--- Running Experiment 2: Goroutines and Channels (Concurrency) ---")

	// Create a channel of type string. Channels are typed, ensuring type safety.
	messages := make(chan string)

	// Launch a goroutine using the 'go' keyword.
	// This anonymous function will run concurrently with the rest of experiment2.
	go func() {
		// Simulate some work being done in the goroutine
		time.Sleep(1 * time.Second)
		// Send a message into the 'messages' channel.
		// This operation blocks until another goroutine is ready to receive.
		messages <- "Hello from goroutine!"
	}()

	fmt.Println("Main goroutine: Waiting for message from another goroutine...")
	// Receive a message from the 'messages' channel.
	// This operation blocks until a message is available in the channel.
	msg := <-messages
	fmt.Printf("Main goroutine: Received: %s\n", msg)

	fmt.Println("\n--- Buffered Channel Example ---")
	// Create a buffered channel with a capacity of 2.
	// This means it can hold up to 2 values before a send operation blocks.
	bufferedChannel := make(chan int, 2)
	go func() {
		for i := 0; i < 5; i++ {
			// Send values to the buffered channel.
			// Sends will not block until the buffer is full (i.e., when sending the 3rd value).
			bufferedChannel <- i
			fmt.Printf("Sender goroutine: Sent: %d\n", i)
		}
		// It's good practice to close a channel when all values have been sent.
		// This signals to receivers that no more values will arrive.
		close(bufferedChannel)
		fmt.Println("Sender goroutine: Channel closed.")
	}()

	fmt.Println("Main goroutine: Receiving values from buffered channel:")
	// Use a 'range' loop to receive values from the channel until it is closed.
	for val := range bufferedChannel {
		fmt.Printf("Main goroutine: Received: %d\n", val)
	}

	fmt.Println("\nPurpose:")
	fmt.Println("- Go's concurrency features are built-in and easy to use.")
	fmt.Println("- Goroutines are cheap to create, allowing for many concurrent tasks.")
	fmt.Println("- Channels provide a safe and synchronized way for goroutines to communicate,")
	fmt.Println("  preventing common concurrency bugs like race conditions.")
	fmt.Println("--- Experiment 2 Finished ---")
}

// experiment3 demonstrates Structs and Methods (Go's approach to Object-Oriented Programming).
// Go uses structs to define composite data types (like classes in other languages)
// and methods (functions with a receiver argument) to associate behavior with these types.
func experiment3() {
	fmt.Println("\n--- Running Experiment 3: Structs and Methods (Object-Oriented Go) ---")

	// Instantiate the globally defined 'Circle' struct.
	// This creates a new Circle value and initializes its Radius field.
	myCircle := Circle{Radius: 5.0}
	fmt.Printf("Created a Circle with Radius: %.2f\n", myCircle.Radius)

	// Call the 'Area()' method on the 'myCircle' instance.
	// This demonstrates how methods are invoked on struct values.
	fmt.Printf("Calculated Area of the Circle: %.2f\n", myCircle.Area())

	fmt.Println("\nPurpose:")
	fmt.Println("- Structs group related data fields together.")
	fmt.Println("- Methods allow you to define behavior that operates on specific struct types.")
	fmt.Println("- This pattern provides encapsulation and code organization similar to OOP,")
	fmt.Println("  without explicit classes or inheritance.")
	fmt.Println("--- Experiment 3 Finished ---")
}

// experiment4 demonstrates Interfaces (Go's approach to Polymorphism).
// Interfaces in Go are implicitly implemented. A type implements an interface
// by simply having all the methods declared in the interface. This allows for
// flexible and decoupled code, where functions can operate on any type that
// satisfies a given interface, regardless of its concrete underlying type.
func experiment4() {
	fmt.Println("\n--- Running Experiment 4: Interfaces (Polymorphism) ---")

	// Create instances of our globally defined structs.
	// Both Circle and Rectangle types have an 'Area()' method,
	// so they both implicitly satisfy the 'Shape' interface.
	circle := Circle{Radius: 7.0}
	rectangle := Rectangle{Width: 4.0, Height: 6.0}

	fmt.Println("Calling printArea with a Circle:")
	// The 'printArea' function accepts a 'Shape' interface.
	// We can pass a 'Circle' type because it implements the 'Area()' method.
	printArea(circle)

	fmt.Println("Calling printArea with a Rectangle:")
	// Similarly, we can pass a 'Rectangle' type.
	printArea(rectangle)

	fmt.Println("\nPurpose:")
	fmt.Println("- Interfaces define a contract: a set of methods that a type must implement.")
	fmt.Println("- Types implicitly satisfy interfaces by having the required methods.")
	fmt.Println("- This enables polymorphism, allowing functions to work with different concrete")
	fmt.Println("  types as long as they adhere to the interface contract.")
	fmt.Println("- It promotes loose coupling and flexible design.")
	fmt.Println("--- Experiment 4 Finished ---")
}

// experiment5 demonstrates the `defer` Statement for Resource Management.
// The `defer` statement schedules a function call to be executed just before
// the surrounding function returns (either normally or due to a panic).
// It's commonly used for cleanup tasks like closing files, releasing locks,
// or closing network connections, ensuring resources are properly managed.
func experiment5() {
	fmt.Println("\n--- Running Experiment 5: Defer Statement (Resource Management) ---")

	fmt.Println("Step 1: Opening a simulated resource (e.g., a file, database connection).")

	// This 'defer' statement will cause "Closing resource (deferred action)."
	// to be printed *after* "Step 2: Performing operations..." and just before
	// the `experiment5` function finishes executing.
	defer fmt.Println("Deferred action: Closing simulated resource.")

	fmt.Println("Step 2: Performing operations with the resource.")
	// Imagine complex logic here that might return early or encounter an error.
	// The deferred call will still execute.

	fmt.Println("\nPurpose:")
	fmt.Println("- Defer ensures that a function call is executed at the very end of the")
	fmt.Println("  current function's execution, regardless of how the function exits.")
	fmt.Println("- This is invaluable for resource cleanup, guaranteeing that resources")
	fmt.Println("  like files, network connections, or locks are properly released,")
	fmt.Println("  even if errors or panics occur.")
	fmt.Println("- It simplifies error handling by centralizing cleanup code.")
	fmt.Println("--- Experiment 5 Finished ---")
}

// experiment6 demonstrates Pointers (Direct Memory Access).
// Go has pointers, which store the memory address of a value.
// They are used to allow functions to modify the value of a variable
// declared in the calling function, or to work with large data structures
// efficiently without copying them.
func experiment6() {
	fmt.Println("\n--- Running Experiment 6: Pointers (Direct Memory Access) ---")

	// Declare an integer variable 'x'.
	x := 10
	fmt.Printf("Initial value of x: %d\n", x)
	// The '&' operator gives the memory address of a variable.
	fmt.Printf("Memory address of x (&x): %p\n", &x)

	// Declare a pointer 'ptr' of type *int (pointer to an integer)
	// and assign it the memory address of 'x'.
	ptr := &x
	fmt.Printf("Value of ptr (which is the address of x): %p\n", ptr)
	// The '*' operator (dereferencing) gives the value stored at the memory address
	// pointed to by the pointer.
	fmt.Printf("Value at the address pointed to by ptr (*ptr): %d\n", *ptr)

	// Modify the value at the memory address pointed to by 'ptr'.
	// This directly changes the value of 'x'.
	*ptr = 20
	fmt.Printf("Value of x after modifying through pointer (*ptr = 20): %d\n", x)

	fmt.Println("\nPurpose:")
	fmt.Println("- Pointers allow functions to modify variables directly in the caller's scope.")
	fmt.Println("- They are used for efficient passing of large data structures to functions,")
	fmt.Println("  avoiding costly copies.")
	fmt.Println("- Unlike C/C++, Go pointers do not support pointer arithmetic, which reduces")
	fmt.Println("  the risk of memory corruption issues.")
	fmt.Println("--- Experiment 6 Finished ---")
}

// experiment7 demonstrates Slices (Dynamic Arrays).
// Slices are a powerful and flexible abstraction built on top of Go's arrays.
// They provide dynamic length, the ability to grow or shrink, and convenient
// operations for common array-like tasks. They are very commonly used.
func experiment7() {
	fmt.Println("\n--- Running Experiment 7: Slices (Dynamic Arrays) ---")

	// 1. Declare and initialize a slice using a slice literal.
	// The underlying array is automatically managed.
	numbers := []int{1, 2, 3, 4, 5}
	fmt.Printf("Initial slice: %v, Length: %d (len()), Capacity: %d (cap())\n", numbers, len(numbers), cap(numbers))

	// 2. Append elements to a slice.
	// 'append' might reallocate the underlying array if capacity is exceeded.
	numbers = append(numbers, 6, 7)
	fmt.Printf("After append (6, 7): %v, Length: %d, Capacity: %d\n", numbers, len(numbers), cap(numbers))

	// 3. Slicing a slice: creating a new slice from a portion of an existing one.
	// This creates a view into the original underlying array.
	subSlice := numbers[1:4] // Elements from index 1 (inclusive) up to 4 (exclusive: 1, 2, 3)
	fmt.Printf("Sub-slice (numbers[1:4]): %v, Length: %d, Capacity: %d\n", subSlice, len(subSlice), cap(subSlice))
	// Modifying subSlice can affect the original numbers slice if capacity allows,
	// because they share the same underlying array.

	// 4. Creating a slice with `make`.
	// `make([]type, length, capacity)` creates a slice of a given type,
	// with an initial length and an optional capacity.
	dynamicSlice := make([]string, 3, 5) // length 3, capacity 5
	dynamicSlice[0] = "apple"
	dynamicSlice[1] = "banana"
	dynamicSlice[2] = "cherry"
	// dynamicSlice[3] = "date" // This would cause a runtime error (index out of bounds)
	fmt.Printf("Dynamic slice (made with make): %v, Length: %d, Capacity: %d\n", dynamicSlice, len(dynamicSlice), cap(dynamicSlice))

	fmt.Println("\nPurpose:")
	fmt.Println("- Slices are Go's primary dynamic array type, highly flexible and efficient.")
	fmt.Println("- They provide a dynamic view into an underlying array, allowing easy growth/shrinkage.")
	fmt.Println("- `len()` gives current elements, `cap()` gives total underlying array capacity.")
	fmt.Println("- They are fundamental for handling collections of data.")
	fmt.Println("--- Experiment 7 Finished ---")
}

// experiment8 demonstrates Maps (Key-Value Pairs / Hash Tables).
// Maps in Go are unordered collections of key-value pairs. Keys must be comparable
// (e.g., strings, numbers, structs that are comparable), and values can be of any type.
// Maps provide efficient lookup, insertion, and deletion operations.
func experiment8() {
	fmt.Println("\n--- Running Experiment 8: Maps (Key-Value Pairs) ---")

	// 1. Declare and initialize a map using a map literal.
	// Keys are strings, values are integers.
	ages := map[string]int{
		"Alice": 30,
		"Bob":   25,
		"Clara": 35,
	}
	fmt.Printf("Initial map: %v\n", ages)

	// 2. Access a value by key.
	fmt.Printf("Alice's age: %d\n", ages["Alice"])

	// 3. Add a new entry to the map.
	ages["David"] = 40
	fmt.Printf("Map after adding David: %v\n", ages)

	// 4. Delete an entry from the map using `delete()`.
	delete(ages, "Bob")
	fmt.Printf("Map after deleting Bob: %v\n", ages)

	// 5. Check if a key exists in the map using the "comma ok" idiom.
	// This is the idiomatic way to distinguish between a zero value and a missing key.
	if age, ok := ages["Clara"]; ok {
		fmt.Printf("Clara's age is %d (Key found)\n", age)
	} else {
		fmt.Println("Clara not found (Key not present).")
	}

	if age, ok := ages["Eve"]; ok {
		fmt.Printf("Eve's age is %d (Key found)\n", age)
	} else {
		fmt.Println("Eve not found (Key not present).")
	}

	// 6. Iterate over a map using a `for...range` loop.
	// Map iteration order is not guaranteed to be the same over multiple runs.
	fmt.Println("\nIterating over map:")
	for name, age := range ages {
		fmt.Printf("%s is %d years old\n", name, age)
	}

	fmt.Println("\nPurpose:")
	fmt.Println("- Maps provide efficient key-value storage and retrieval.")
	fmt.Println("- The 'comma ok' idiom (`value, ok := map[key]`) is crucial for checking")
	fmt.Println("  key existence and avoiding confusion with zero values.")
	fmt.Println("- They are widely used for dictionaries, lookups, and counting.")
	fmt.Println("--- Experiment 8 Finished ---")
}

// experiment9 demonstrates Variadic Functions (Flexible Arguments).
// A variadic function can accept zero or more arguments of a specific type.
// This is denoted by `...` before the type in the function signature.
// Inside the function, the variadic parameter becomes a slice of that type.
func experiment9() {
	fmt.Println("\n--- Running Experiment 9: Variadic Functions (Flexible Arguments) ---")

	// Define an anonymous function `sum` that is variadic.
	// It accepts any number of `int` arguments.
	sum := func(numbers ...int) int {
		total := 0
		// The 'numbers' parameter inside the function is treated as a slice of int.
		for _, num := range numbers {
			total += num
		}
		return total
	}

	// Calling the variadic function with different numbers of arguments.
	fmt.Printf("Sum of 1, 2, 3: %d\n", sum(1, 2, 3))
	fmt.Printf("Sum of 10, 20, 30, 40, 50: %d\n", sum(10, 20, 30, 40, 50))
	fmt.Printf("Sum of no numbers: %d\n", sum()) // Can be called with zero arguments

	// Passing a slice to a variadic function.
	// Use the `...` operator after the slice to "unfurl" its elements
	// into individual arguments for the variadic parameter.
	nums := []int{100, 200, 300}
	fmt.Printf("Sum of slice %v: %d\n", nums, sum(nums...))

	fmt.Println("\nPurpose:")
	fmt.Println("- Variadic functions provide flexibility, allowing a function to accept")
	fmt.Println("  a varying number of arguments of the same type.")
	fmt.Println("- This is useful for functions like `fmt.Println` or `sum` where the")
	fmt.Println("  number of inputs isn't fixed.")
	fmt.Println("- Inside the function, variadic arguments are treated as a slice.")
	fmt.Println("--- Experiment 9 Finished ---")
}

// experiment10 demonstrates the init() Function (Package Initialization).
// The `init()` function is a special, automatically executed function within a package.
// It runs before any other function in the package (including main) and is used for
// one-time initialization tasks, setting up package-level variables, or verifying conditions.
// A package can have multiple `init()` functions (across different files), and they are
// executed in the order of import path declaration.
func experiment10() {
	fmt.Println("\n--- Running Experiment 10: init() Function (Package Initialization) ---")

	// This check demonstrates that 'initRan' was set to true before main() even started,
	// proving the `init()` function's execution order.
	if initRan {
		fmt.Println("Confirmation: The global 'initRan' variable is true.")
		fmt.Println("This means the init() function was executed automatically and successfully")
		fmt.Println("before the main() function (and thus before any experiment could be selected).")
	} else {
		fmt.Println("Error: The init() function did not run as expected.")
	}

	fmt.Println("\nPurpose:")
	fmt.Println("- `init()` functions are used for bootstrapping or setting up initial state.")
	fmt.Println("- They run automatically and only once per package initialization.")
	fmt.Println("- Useful for tasks like registering drivers, parsing configuration files,")
	fmt.Println("  or performing sanity checks before the main program logic begins.")
	fmt.Println("--- Experiment 10 Finished ---")
}
