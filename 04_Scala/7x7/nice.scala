// Defines a class 'Personage' with a primary constructor that takes a single parameter 'name'.
class Personage(val name: String)

// Defines a trait 'Nice'. Traits in Scala are similar to interfaces in Java, but they can contain concrete methods.
trait Nice {
  // Defines a method 'greet' that prints a friendly message to the console.
  def greet(): Unit = println("Howdily doodily.")
}

// Defines a class 'Character' that inherits from both 'Personage' and 'Nice'.
// It overrides the 'name' parameter from the 'Personage' class.
class Character(override val name: String) extends Personage(name) with Nice

// Defines an object 'MainNice', which is a singleton object that acts as the entry point of the program.
object MainNice {
  // The 'main' method is the entry point of the program.
  def main(args: Array[String]): Unit = {
    // Creates an instance of 'Character' with the name "Ned".
    val flanders = new Character("Ned")
    // Calls the 'greet' method on the 'flanders' instance, which will print the message "Howdily doodily."
    flanders.greet() // Method invocation with parentheses
  }
}
