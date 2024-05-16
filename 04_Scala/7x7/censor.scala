// Define a trait named Censor
trait Censor {
  // A Map to hold curse words and their alternatives
  val curseWords = Map("Shoot" -> "Pucky", "Darn" -> "Beans")

  // Method to replace curse words in a given text with alternatives
  def replaceCurseWords(text: String): String = {
    // Use foldLeft to iterate over the map and replace each curse word
    curseWords.foldLeft(text) { case (acc, (curse, alternative)) =>
      // Replace all occurrences of the curse word with the alternative
      acc.replaceAll("\\b" + curse + "\\b", alternative)
    }
  }
}

// Define an object that extends the Censor trait
object TextCleaner extends Censor {
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

