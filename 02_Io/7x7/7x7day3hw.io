// Day 3 Self-Study (Homework)
// Do:
// 1. Enhance the XML program to add spaces to show the indentation structure.
// 2. Create a list syntax that uses brackets.
// 3. Enhance the XML program to handle attributes: if the first argument is a map (use the curly brackets syntax), add attributes to
// the XML program. For example: book({"author": "Tate"}...) would print <book author="Tate">:

// 1. Enhance the XML program to add spaces to show the indentation structure.
// Clone the Object prototype to create a new Builder object
// Before enhancing
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
// After enhancing (comments marked with **** to show changes to original)
// Clone the Object prototype to create a new Builder object
Builder := Object clone
// Add an indentation level property to the Builder ****
Builder indentLevel := 0
// Define a 'forward' method on the Builder object
// This method will be called when a message is sent to Builder that it does not understand
Builder forward := method(
    // Increase the indentation level ****
    Builder indentLevel = Builder indentLevel + 1
    // Write the opening tag with the name of the unknown message with proper indentation and check indent level as level one needs no indent ****
    writeln(if(Builder indentLevel == 1, "" , (Builder indentLevel - 1) repeat(" ")), "<", call message name, ">")
    // Iterate over the arguments of the unknown message
    call message arguments foreach(arg,
        // Send each argument as a message to self (Builder) and store the result in 'content'
        content := self doMessage(arg)
        // If the result 'content' is a Sequence (string), write it with indentation and check indent level as level one needs no indent ****
        if(content type == "Sequence", writeln(if(Builder indentLevel == 1, "" , (Builder indentLevel - 1) repeat(" ")), content))
    )
    // Write the closing tag with the name of the unknown message with proper indentation and check indent level as level one needs no indent ****
    writeln(if(Builder indentLevel == 1, "" , (Builder indentLevel - 1) repeat(" ")), "</", call message name, ">")
    // Decrease the indentation level ****
    Builder indentLevel = Builder indentLevel - 1
)
// Use the Builder to construct an unordered list (ul) with list items (li)
// Each 'li' message is forwarded to the 'forward' method, which handles it
Builder ul(
    li("Io"),
    li("Lua"),
    li("JavaScript")
)
// Reset the indentation level after building the HTML structure ****
Builder indentLevel := 0
"" println
//


// 2. Create a list syntax that uses brackets. // TODO
//
// Define a method to interpret the custom list syntax





// 3. Enhance the XML program to handle attributes: // TODO
//
//
