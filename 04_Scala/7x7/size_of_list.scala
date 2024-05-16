// Define an object Main that will contain the main method
object Main {
  // Define the main method which is the entry point of the program
  def main(args: Array[String]): Unit = {
    // Create a List of Strings
    val listOfStrings: List[String] = List("hello", "world", "scala", "programming")
    // Use foldLeft to iterate over the list and calculate the total length of all strings
    val totalSize: Int = listOfStrings.foldLeft(0)((sum, currentString) => sum + currentString.length)
    // Print the total size of all strings in the list
    println(totalSize)
  }
}
