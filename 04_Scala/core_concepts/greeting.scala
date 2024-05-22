// Define a class named 'Person'
class Person(val name: String) {
  // Constructor of the class 'Person' takes one parameter 'name' which is a String

  // Define a method 'greet' that prints a greeting to the console
  def greet(): Unit = {
    // Print a formatted string to the console using string interpolation
    // The '$name' syntax will replace '$name' with the value of the 'name' property of the object
    println(s"Hello, my name is $name!")
  }
}

// Define the 'main' method which is the entry point of the program
def main(args: Array[String]): Unit = {
    // Create a new instance of 'Person' with the name 'Socks'
    val person = new Person("Socks")
    // Call the 'greet' method on the 'person' instance, which will execute the code within the 'greet' method
    person.greet()
}