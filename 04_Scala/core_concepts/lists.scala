object Main {
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3)

    list match {
      case List(1, 2, 3) => println("List contains 1, 2, 3")
      case List(1, _, _) => println("List at least starts with a 1")
      case _ => println("This is a different list")
    }
  }
}
