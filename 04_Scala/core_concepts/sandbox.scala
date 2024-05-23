// Code samples from the Learn to Code \\ Code to Learn 
// Copy into the appropriate scala 'container' to run
@main def sandbox(): Unit = {

  // Singleton object with a property
  object AppConfig: 
    val appName: String = "My Scala Application"
    def printAppName(): Unit = println(appName)


  // Usage
  AppConfig.printAppName() // Outputs: My Scala Application


  // Class and its companion object
  class Circle(val radius: Double)

  object Circle:
    def calculateArea(radius: Double): Double = Math.PI * radius * radius
  

  // Usage
  val circleArea = Circle.calculateArea(5)
  println(s"Area of the circle: $circleArea")


  // Object with apply method
  object Person:
    def apply(name: String, age: Int): Person = new Person(name, age)
  

  class Person(val name: String, val age: Int)

  // Usage
  val john = Person("John", 30)               // Calls Person.apply
  println(s"Person's name: ${john.name}, age: ${john.age}")

  // Object extending a trait
  trait Greeter:
    def greet(name: String): Unit
  

  object EnglishGreeter extends Greeter:
    def greet(name: String): Unit = println(s"Hello, $name!")
  

  // Usage
  EnglishGreeter.greet("Alice") // Outputs: Hello, Alice!

  // Conditionals - if / else and if / else if / else
  val x = 1
  if x == 1 then println(x)

  if x == 1 then
    println("x is 1, as you can see:")
    println(x)
  

  if x == 1 then
    println("x is 1, as you can see:")
    println(x)
  else
    println("x was not 1")
  

  if x == 1 then println("x is 1") else println("x was not 1")

  if (x > 0) then
    println("x is positive")
  else if x < 0 then 
    println("x is negative")
  else
    println("x is zero")
  
  // Conditionals - Expressions
  // You can assign the result to a variable
  val a = 1
  val b = 2
  val minValue = if (a < b) a else b

  // You can also use them as method bodies
  def compare(a: Int, b: Int): Int = 
    if a < b then -1 else if a == b then 0 else 1


  // Conditionals - For loops and expressions
  // Your basic for loop in Scala
  val ints = List(1, 2, 3, 4, 5)
  for (i <- ints) println(i)

  // Using guards
  for (i <- ints) if i > 2 then println(i)

  // for expressions
  val doubles = for (i <- ints) yield i * 2


  // Conditionals - Pattern matching
  val number = 3
  number match
    case 1 => println("One")
    case 2 => println("Two")
    case 3 => println("Three")
    case _ => println("Something else")
  

  // Conditionals - Type matching
  def describeType(x: Any): String = x match
    case i: Int => "This is an integer."
    case s: String => "This is a string."
    case b: Boolean => "This is a boolean."
    case _ => "This is another type."
  

  // Conditionals - Case class matching
  case class Personage(name: String, age: Int)

  val person = Personage("Alice", 25)

  person match
    case Personage("Alice", 25) => println("Hello, Alice!")
    case Personage(name, age) => println(s"Name: $name, Age: $age")
  


  // Conditionals - matching on lists
  val list = List(1, 2, 3)

  list match
    case List(1, 2, 3) => println("List contains 1, 2, 3")
    case List(1, _, _) => println("List starts with 1") 
    case _ => println("This is a different list")

  // Conditionals Matching with Guards
  val anumber = 10

  anumber match
    case a if a > 0 => println("Positive number")
    case a if a == 0 => println("Zero")
    case a if a < 0 => println("Negative number")
  

  // for do
  // Iterating over a collection with a single line
  for i <- 1 to 5 do println(i)

  // Using a multiline for loop with a condition
  for
    i <- 1 to 10
    if i % 2 == 0
  do
    println(s"Even number: $i")
  end for // 'end for' can be used to improve readability

  // for yield
  // Generating a new collection with for/yield
  val numbers = List(1, 2, 3, 4, 5)
  val squares = for n <- numbers yield n * n
  println(squares)                // List(1, 4, 9, 16, 25)


  // A simple while loop
  var i = 5
  while i > 0 do
    println(i)
    i -= 1

  // Abstraction using FP
  // A higher-order function that takes a function as a parameter
  def operateOnNumbers(a: Int, b: Int, operation: (Int, Int) => Int): Int = 
    operation(a, b)
  

  // Using an anonymous function with the higher-order function
  val sum = operateOnNumbers(5, 10, (x, y) => x + y)

  // Partial function application
  val multiplyByTwo: Int => Int = operateOnNumbers(_, 2, _ * _)

  // Currying
  def curriedAdd(a: Int)(b: Int): Int = a + b
  val addFive = curriedAdd(5)

  // Abstraction using OOP
  // Define a trait with an abstract method
  trait Drivable:
    def drive(): Unit
  

  // Define a class that extends the trait and provides an implementation
  class Car extends Drivable:
    override def drive(): Unit = println("Driving a car")
  

  // Define an object that uses the class
  object Vehicle:
    val myCar: Drivable = new Car
    
    def startJourney(): Unit = 
      myCar.drive()
    
  

  // Method vs Functions
  // Method inside a class
  class MathOps: 
    def multiplyMethod(a: Int, b: Int): Int = a * b
  

  // Function as a value
  val multiplyFunction: (Int, Int) => Int = (a, b) => a * b

  // Using the method
  val ops = new MathOps()
  val resultMethod = ops.multiplyMethod(2, 3)

  // Using the function
  val resultFunction = multiplyFunction(2, 3)

  // Factorial as a function
  lazy val factorialFunction: Int => BigInt = n => 
    if (n == 0) 1
    else n * factorialFunction(n - 1)
  

  // Factorial as a method
  def factorialMethod(n: Int): BigInt = 
    if (n == 0) 1
    else n * factorialMethod(n - 1)
  

  // Strings
  val singleLineString = "Hello, I am a single line string"
  val multiLineString = """Hello, I am
                          |a multiline
                          |String""".stripMargin

  // String concatenation
  val askTheTimeString = "...What time is it?"
  val concatStr = singleLineString + askTheTimeString

  // String interpolation
  val age = 30
  val agePresentation = s"I am $age years old"

  // Regular expression matching
  import scala.util.matching.Regex
  val regEx: Regex = "^(?=.*[a-zA-Z])(?=.*[0-9])".r
  val testString = "this is a string with numbers 123456"
  val result = regEx.findFirstMatchIn(testString).isDefined

  // Creating lists
  val fruit: List[String] = List("apples", "oranges", "pears")
  val nums: List[Int] = List(1, 2, 3, 4)
  val empty: List[Nothing] = List()

  // Accessing elements
  val firstFruit = fruit.head // "apples"
  val remainingFruits = fruit.tail // List("oranges", "pears")
  val secondFruit = fruit(1)		// “oranges”


  // Pattern matching
  fruit match
    case head :: tail => println(s"Head: $head, Tail: $tail")
    case Nil => println("The list is empty")
  

  // Concatenating lists
  val moreFruit = "bananas" :: fruit // List("bananas", "apples", "oranges", "pears")
  val lotsOfFruit = fruit ::: moreFruit // List("apples", "oranges", "pears", "bananas", "apples", "oranges", "pears")

  // list functions
  val lengths = fruit.map(_.length) // List(6, 7, 5)
  val shortFruit = fruit.filter(_.length <= 5) // List("pears")

  // foreach example
  val numberslist = List(1, 2, 3, 4, 5)

  // Print each number
  numberslist.foreach(println)

  // Print the double of each number
  numberslist.foreach(num => println(num * 2))

  // Conditional Printing
  numberslist.foreach(num => if (num % 2 == 0) println(s"$num is even"))

  // Summing numbers
  var total = 0
  numberslist.foreach(total += _)
  println(total)


  // Ranges
  val rangeIncl = Range.inclusive(1, 10)
  for(i <- 1 to 10)
    println(i)
  
  val listFromRange = (1 to 10).toList
  val arrayFromRange = (1 to 10).toArray
  val setFromRange = (1 to 10).toSet
  val evenNumbers = 2 to 10 by 2
  val alphabetRange = 'a' to 'z'
  val reverseRange = 10 to 1 by -1


  // Reading from the console using scala.io.StdIn
  import scala.io.StdIn.readLine

  print("Please enter your name: ")
  val name = readLine()
  println(s"Hello, $name!")

  def gcdSubtraction(a: Int, b: Int): Int = {
    var x = a
    var y = b
    while (x != y) {
      if (x > y) x = x - y
      else y = y - x
    }
    x 
  }

  val gcd = gcdSubtraction(48, 18)
  println(s"The GCD is: $gcd")

  def gcdModuloRecursive(a: Int, b: Int): Int = {
    var x = a
    var y = b
    while (y != 0) {
      val temp = y
      y = x % y
      x = temp
    }
    x // x holds the GCD when y becomes 0
  }

  // Example usage:
  val gcdmod = gcdModuloRecursive(48, 18)
  println(s"The GCD is: $gcdmod")


}