// Define a class named 'APerson' with a constructor that takes a 'name' parameter.
class APerson(val name: String) {
  // A method 'talk' that prints a message to the console.
  def talk(message: String): Unit = println(s"$name says $message")
  // A method 'id' that returns the person's name.
  def id: String = name
}

// Define a class 'Employee' that inherits from 'APerson'.
class Employee(override val name: String, val number: Int) extends APerson(name) {
  // Override the 'talk' method to include the employee's number in the printed message.
  override def talk(message: String): Unit = {
    println(s"$name with number $number says $message")
  }
  // Override the 'id' method to return the employee's number as a string.
  override def id: String = number.toString
}

// Define an 'object' named 'Main' which is a singleton object.
object Main {
  // The 'main' method, which is the entry point of the program.
  def main(args: Array[String]): Unit = {
    // Create an instance of 'Employee' with the name "Yoda" and number 4.
    val employee = new Employee("Yoda", 4)
    // Call the 'talk' method on the 'employee' instance.
    employee.talk("Extend or extend not. There is no try.")
  }
}