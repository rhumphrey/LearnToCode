class Personage(val name: String)

trait Nice {
  def greet(): Unit = println("Howdily doodily.")
}

class Character(override val name: String) extends Personage(name) with Nice



object MainNice {
  def main(args: Array[String]): Unit = {
    val flanders = new Character("Ned")
    flanders.greet() // Method invocation with parentheses
  }
}