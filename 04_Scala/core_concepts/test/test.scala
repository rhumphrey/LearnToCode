import scala.util.matching.Regex

// Define the main method at the top level
@main def run(): Unit =
  import scala.io.Source

  // Open the file
  val filename = "example.txt"
  val bufferedSource = Source.fromFile(filename)

  // Read the file line by line
  for (line <- bufferedSource.getLines)
    println(line)


  // Don't forget to close the source
  bufferedSource.close()

