// Import the Scala I/O library for reading files
import scala.io.Source

// Define a trait named CensorV2
trait CensorV2 {
  // Declare a Map to hold curse words and their alternatives
  val curseWords: Map[String, String] = {
    // Open the file 'cursewords.txt' to read curse words and their alternatives
    val source = Source.fromFile("cursewords.txt")
    // Read all lines from the file and convert to a List
    val lines = source.getLines().toList
    // Close the file to free resources
    source.close()
    // Split each line by comma, trim whitespace, and create a Map entry
    lines.map { line =>
      val parts = line.split(",")
      (parts(0).trim -> parts(1).trim)
    }.toMap // Convert the List of tuples to a Map
  }

  // Method to replace curse words in a given text with alternatives
  def replaceCurseWords(text: String): String = {
    // Use foldLeft to iterate over the map and replace each curse word
    curseWords.foldLeft(text) { case (acc, (curse, alternative)) =>
      // Replace all occurrences of the curse word with the alternative
      acc.replaceAll("\\b" + curse + "\\b", alternative)
    }
  }
}

// Define an object that extends the CensorV2 trait
object TextCleanerV2 extends CensorV2 {
  // Main method - entry point of the program
  def main(args: Array[String]): Unit = {
    // Original text containing curse words
    val originalText = "Oh Shoot, I can't believe we lost! Darn it!"
    // Clean the text by replacing curse words
    val cleanedText = replaceCurseWords(originalText)
    // Print the cleaned text
    println(cleanedText)
  }
}
