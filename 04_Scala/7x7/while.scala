// Define a singleton object named 'While'
object While {
  // Define a method 'whileLoop' that returns no meaningful value (Unit)
  def whileLoop: Unit = {
    // Initialize a variable 'i' with the value 1
    var i = 1
    // Continue looping as long as 'i' is less than or equal to 3
    while(i <= 3) {
      // Print the current value of 'i' to the console
      println(i)
      // Increment the value of 'i' by 1
      i += 1
    }
  }

  // Define the main method that serves as the entry point of the program
  def main(args: Array[String]): Unit = {
    // Call the 'whileLoop' method to execute the loop
    whileLoop
  }
}

