class Person(first_name: String) {
  println("Outer constructor")
  
  // Primary constructor
  def this(first_name: String, last_name: String) = {
    this(first_name) // Calls the primary constructor
    println("Inner constructor")
  }
  
  def talk(): Unit = println("Hi")
}

// Creating instances of Person
def main(args: Array[String]): Unit = {
    val bob = new Person("Bob")
    val bobTate = new Person("Bob", "Tate")
}

