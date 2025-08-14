# Crystal Coding FAQs and Experiments

## Lab setup (Windows)

- Install the official Crystal runtime/compiler for Windows by downloading the installer from the official Crystal-lang.org website.
- Open Command Prompt or PowerShell.
- Check version: `crystal --version`
- Create a folder `lab-Crystal` (e.g., `mkdir lab-Crystal`) and navigate into it (`cd lab-Crystal`).
- Run a file from the shell: `crystal run your_file_name.cr`
- Optional: To quickly check installation in a shell, type `crystal eval "puts 1 + 1"` and press Enter.

## 10 Common Coding FAQs

1. Q: How do I display output to the console in Crystal?
    A: Use the `puts` method (short for "put string") to print a string followed by a newline, or `print` for output without a newline.
2. Q: How do I declare variables in Crystal?
    A: Variables are declared by assigning a value; Crystal uses type inference, so explicit type declarations are often optional but can be added with a colon (`:`) followed by the type.
3. Q: What are the basic data types available in Crystal?
    A: Common basic types include `Int` (integers like `Int32`, `Int64`), `Float` (floating-point numbers like `Float32`, `Float64`), `Bool` (true/false), `Char` (single characters), and `String` (sequences of characters).
4. Q: How can I get user input from the console?
    A: Use `STDIN.gets` to read a line from standard input, which returns a `String?` (a `String` or `nil`); you'll typically chain `.not_nil!` and `.strip` to get a non-nil, trimmed string.
5. Q: How do I write conditional statements (if/else) in Crystal?
    A: Crystal uses `if`, `elsif`, and `else` keywords, similar to Ruby, to execute code blocks based on conditions.
6. Q: What are the common ways to loop or iterate in Crystal?
    A: You can use `while` loops for condition-based iteration, or `each` with ranges or collections for iterating over elements.
7. Q: How do I define and call a function (or method) in Crystal?
    A: Define functions using the `def` keyword, optionally specifying argument types and return type, and call them by their name followed by parentheses containing arguments.
8. Q: How does Crystal handle basic errors or exceptions?
    A: Crystal uses `begin...rescue...end` blocks to catch and handle exceptions that might occur during code execution.
9. Q: What is type inference in Crystal?
    A: Type inference is Crystal's ability to automatically determine the data type of a variable or expression at compile time, reducing the need for explicit type annotations.
10. Q: How do I work with collections like arrays in Crystal?
    A: Arrays are ordered, mutable collections of elements, declared using square brackets `[]`, and can hold elements of a specific type or a union of types.

## 10 Tiny Experiments

1. Goal: Explore FAQ 1 (Display output)
    Filename: `print_output.cr`
    Description: Prints a simple "Hello, Crystal!" message to the console.
    How to run (Windows): `crystal run print_output.cr`

    ```crystal
    puts "Hello, Crystal!"
    print "This is on the same line, "
    puts "as this next bit."
    puts "And this is after it."
    ```

2. Goal: Explore FAQ 2 (Define variables)
    Filename: `define_variables.cr`
    Description: Demonstrates declaring variables with and without explicit type annotations.
    How to run (Windows): `crystal run define_variables.cr`

    ```crystal
    name = "Crystal" # Type inferred as String
    version = 1.0 # Type inferred as Float64
    is_compiled : Bool = true # Explicit type annotation

    puts "Language: #{name} (Type: #{typeof(name)})"
    puts "Version: #{version} (Type: #{typeof(version)})"
    puts "Compiled: #{is_compiled} (Type: #{typeof(is_compiled)})"
    ```

3. Goal: Explore FAQ 3 (Basic data types)
    Filename: `basic_data_types.cr`
    Description: Shows examples of various fundamental data types and their inferred types.
    How to run (Windows): `crystal run basic_data_types.cr`

    ```crystal
    integer_val = 123
    float_val = 4.56
    boolean_val = false
    char_val = 'X'
    string_val = "Example"

    puts "Integer: #{integer_val} (Type: #{typeof(integer_val)})"
    puts "Float: #{float_val} (Type: #{typeof(float_val)})"
    puts "Boolean: #{boolean_val} (Type: #{typeof(boolean_val)})"
    puts "Character: #{char_val} (Type: #{typeof(char_val)})"
    puts "String: #{string_val} (Type: #{typeof(string_val)})"
    ```

4. Goal: Explore FAQ 4 (Get user input)
    Filename: `get_user_input.cr`
    Description: Prompts the user for their name and then prints a greeting.
    How to run (Windows): `crystal run get_user_input.cr`

    ```crystal
    puts "Please enter your name:"
    user_name = STDIN.gets.not_nil!.strip # Read input, ensure not nil, remove whitespace
    puts "Hello, #{user_name}! Welcome to Crystal."
    ```

5. Goal: Explore FAQ 5 (Conditional statements)
    Filename: `conditionals.cr`
    Description: Uses an `if-else` statement to check if a number is positive or negative.
    How to run (Windows): `crystal run conditionals.cr`

    ```crystal
    number = -5

    if number > 0
      puts "#{number} is a positive number."
    elsif number == 0
      puts "#{number} is zero."
    else
      puts "#{number} is a negative number."
    end
    ```

6. Goal: Explore FAQ 6 (Loops)
    Filename: `loops_example.cr`
    Description: Demonstrates a `while` loop counting up and an `each` loop iterating over a range.
    How to run (Windows): `crystal run loops_example.cr`

    ```crystal
    i = 0
    puts "While loop:"
    while i < 3
      puts "  Iteration #{i}"
      i += 1
    end

    puts "\nEach loop over a range:"
    (1..3).each do |num|
      puts "  Number: #{num}"
    end
    ```

7. Goal: Explore FAQ 7 (Define a function)
    Filename: `define_function.cr`
    Description: Defines a simple function to add two integers and calls it.
    How to run (Windows): `crystal run define_function.cr`

    ```crystal
    def add_numbers(a : Int32, b : Int32) : Int32
      a + b # The last expression is implicitly returned
    end

    result = add_numbers(10, 25)
    puts "The sum of 10 and 25 is: #{result}"
    ```

8. Goal: Explore FAQ 8 (Basic error handling)
    Filename: `error_handling.cr`
    Description: Shows how to use `begin...rescue` to handle a potential conversion error.
    How to run (Windows): `crystal run error_handling.cr`

    ```crystal
    input_str = "not_a_number"

    begin
      # Attempt to convert a string to an integer
      num_val = input_str.to_i
      puts "Successfully converted '#{input_str}' to #{num_val}"
    rescue ex : ArgumentError
      # Catch a specific ArgumentError if conversion fails
      puts "Caught an error: Could not convert '#{input_str}' to a number. Message: #{ex.message}"
    rescue ex
      # Catch any other unexpected error
      puts "Caught an unexpected error: #{ex.message}"
    end
    ```

9. Goal: Explore FAQ 9 (Type inference)
    Filename: `type_inference_demo.cr`
    Description: Illustrates how Crystal automatically infers the types of variables based on their assigned values.
    How to run (Windows): `crystal run type_inference_demo.cr`

    ```crystal
    inferred_text = "Hello, Crystal!" # Inferred as String
    inferred_count = 100 # Inferred as Int32
    inferred_price = 9.99 # Inferred as Float64
    inferred_status = true # Inferred as Bool

    puts "inferred_text is of type: #{typeof(inferred_text)}"
    puts "inferred_count is of type: #{typeof(inferred_count)}"
    puts "inferred_price is of type: #{typeof(inferred_price)}"
    puts "inferred_status is of type: #{typeof(inferred_status)}"
    ```

10. Goal: Explore FAQ 10 (Arrays/Collections)
    Filename: `arrays_example.cr`
    Description: Creates an array of strings, adds an element, and accesses elements.
    How to run (Windows): `crystal run arrays_example.cr`

    ```crystal
    fruits = ["apple", "banana", "cherry"] # Array of String
    puts "Initial fruits: #{fruits}"

    fruits << "date" # Add an element to the end
    puts "Fruits after adding 'date': #{fruits}"

    puts "First fruit: #{fruits[0]}" # Access element by index
    puts "Number of fruits: #{fruits.size}" # Get array size
    ```
