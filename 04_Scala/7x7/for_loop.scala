// Define an object named ForLoop. In Scala, an object is a singleton instance of a class.
object ForLoop {
  // Define a method named forLoop that takes an array of Strings as an argument and returns nothing (Unit).
  def forLoop(args: Array[String]): Unit = {
    // Print a line to the console to indicate the start of a for loop.
    println("for loop using Java-style iteration")
    // Iterate over the indices of the args array using a for loop.
    for(i <- args.indices) {
      // Print each element of the args array by its index.
      println(args(i))
    }
  }

  // Define the main method, which is the entry point of the program.
  // It also takes an array of Strings as an argument and returns nothing (Unit).
  def main(args: Array[String]): Unit = {
    // Call the forLoop method and pass the args array to it.
    forLoop(args)
  }
}
