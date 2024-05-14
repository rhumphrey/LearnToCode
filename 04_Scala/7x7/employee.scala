class APerson(val name: String) {
  def talk(message: String): Unit = println(s"$name says $message")
  def id: String = name
}

class Employee(override val name: String, val number: Int) extends APerson(name) {
  override def talk(message: String): Unit = {
    // Corrected the string interpolation by removing the backslashes
    println(s"$name with number $number says $message")
  }
  override def id: String = number.toString
}

object Main {
  def main(args: Array[String]): Unit = {
    val employee = new Employee("Yoda", 4)
    employee.talk("Extend or extend not. There is no try.")
  }
}