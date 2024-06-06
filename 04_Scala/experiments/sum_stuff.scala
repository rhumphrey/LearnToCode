import scala.io.StdIn.readLine

@main def sumStuff(): Unit = 
  println("Enter a list of numbers separated by spaces:")
  val numbers = readLine().split(" ").map(_.toInt).toList

  val sum = numbers.sum
  val average = sum.toDouble / numbers.size

  average match 
    case avg if avg > 10 => println(s"The average is greater than 10. It's $average.")
    case 10              => println("The average is exactly 10.")
    case _               => println(s"The average is less than 10. It's $average.")


