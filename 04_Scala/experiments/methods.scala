// Based on examples from https://docs.scala-lang.org/scala3/book/taste-methods.html
//                        https://docs.scala-lang.org/scala3/book/methods-most.html
@main def methods() =
  /* 
  The basic syntax of a methods
  def methodName(param1: Type1, param2: Type2): ReturnType =
    - the method body goes here
  */
  
  // An example
  def sum(a: Int, b: Int): Int = a + b
  def concatenate(s1: String, s2: String): String = s1 + s2

  // Use cases
  val firstValue = 3
  val secondValue = 5
  val theSum = sum(firstValue, secondValue)
  println(s"The sum of $firstValue and $secondValue is $theSum")
  val firstString = "Hi! "
  val secondString = "Yowlie"
  val theWholeString = concatenate(firstString, secondString)
  println(s"If you have the strings $firstString and $secondString then the whole string is $theWholeString")

  /* 
  You don’t have to declare a method’s return type (but it's advised), so you can write those methods like this
  def sum(a: Int, b: Int) = a + b
  def concatenate(s1: String, s2: String) = s1 + s2
   */

  //  Multiline method example
  import java.io.{StringWriter, PrintWriter}
  def getStackTraceAsString(t: Throwable): String =
    val sw = new StringWriter                       // Create a StringWriter to capture the stack trace as a string
    t.printStackTrace(new PrintWriter(sw))          // Invoke the printStackTrace method on the provided Throwable object (t) and writes the stack trace to the specified PrintWriter (sw)
    sw.toString                                     // Convert the contents of the StringWriter to a String and return it

  // Method parameters can have default values
  def makeConnection(url: String, timeout: Int = 5000): Unit =
    println(s"url=$url, timeout=$timeout")

  // This makeConnection method can now be called a couple of ways   
  makeConnection("https://localhost")         // url=http://localhost, timeout=5000
  makeConnection("https://localhost", 2500)   // url=http://localhost, timeout=2500

  // Scala supports the use of named parameters when calling a method, so you can aslo call the makeConnection method this way
  makeConnection(
    url = "https://localhost",
    timeout = 2500
  )

  // This is very useful when you might have a method with multiple parameters of the same type
  // doTheThing(true, true, true, false)

  // The extension keyword 
  // It declares that you’re about to define one or more extension methods on the parameter that’s put in parentheses.
  // In the following example the parameter s of type String can then be used in the body of your extension methods.
  /* 
  1. Method Signature:
     - The code defines an extension method on the `String` type.
     - The method is named `makeInt`.
     - It takes a single parameter named `radix` of type `Int`.
     - The return type of the method is `Int`.

  2. Method Body:
     - The method body consists of a single expression:
       - `Integer.parseInt(s, radix)`
     - This expression invokes the `parseInt` method from the `Integer` class, passing in two arguments:
       - `s`: The input string that represents an integer value.
       - `radix`: The base (or radix) in which the input string is interpreted (e.g., decimal, hexadecimal, etc.).

  3. Explanation:
     - The purpose of this extension method is to convert a string representation of an integer (in a specific radix) into an actual integer value.
     - Here's how it works:
       - Given a string `s`, it attempts to parse it as an integer using the specified `radix`.
       - The `radix` determines the numbering system (e.g., base 10 for decimal, base 16 for hexadecimal).
       - The method returns the parsed integer value.
     - For example:
       - If `s` is `"1010"` and `radix` is `2`, the method will interpret it as a binary number and return `10` (in decimal).
       - If `s` is `"FF"` and `radix` is `16`, the method will interpret it as a hexadecimal number and return `255` (in decimal).
  */

  extension (s: String)
    def makeInt(radix: Int): Int = Integer.parseInt(s, radix)
  println("1".makeInt(2))      // Int = 1
  println("10".makeInt(2))     // Int = 2
  println("100".makeInt(2))    // Int = 4

  /* 
  Methods that take no parameters
  When a method takes no parameters, it’s said to have an arity level of arity-0. 
  Similarly, when a method takes one parameter it’s an arity-1 method. When you create arity-0 methods:
  - If the method performs side effects, such as calling println, declare the method with empty parentheses
  - If the method does not perform side effects—such as getting the size of a collection, 
    which is similar to accessing a field on the collection—leave the parentheses off
   */

  // Using if as a method body
  def isTruthy(a: Any) =
  if a == 0 || a == "" || a == false then
    false
  else
    true

  // In use
  println(isTruthy(0))
  println(isTruthy(""))     
  println(isTruthy("hi"))   
  println(isTruthy(1.0))    

  // Using match as a method body
  def isTruthyCase(a: Matchable) = a match
  case 0 | "" | false => false
  case _ => true

  // In use
  println(isTruthyCase(0))
  println(isTruthyCase(""))     
  println(isTruthyCase("hi"))   
  println(isTruthyCase(1.0)) 

  // Controlling visibility in classes
  // In classes, objects, traits, and enums, Scala methods are public by default
  class Dog:
    def speak() = println("Woof")

  val d = new Dog
  d.speak() 

  // Methods can also be marked as private. 
  // This makes them private to the current class, so they can’t be called nor overridden in subclasses
  class Animal:
    private def breathe() = println("I'm breathing")

  class Cat extends Animal:
    // this method won’t compile
    // override def breathe() = println("Yo, I'm totally breathing") // uncomment to see error
    def speak() = println("Yowl")
  
  val c = new Cat
  c.speak()

  // If you want to make a method private to the current class and also allow subclasses to 
  // call it or override it, mark the method as protected, as shown with the speak method
  class Animal2:
    private def breathe() = println("I’m breathing")
    def walk() =
      breathe()
      println("I’m walking")
    protected def speak() = println("Hello?")

  class Cat2 extends Animal2:
    override def speak() = println("Meow")

  val cat = new Cat2
  cat.walk()
  cat.speak()
  // cat.breathe()   // won’t compile because it’s private uncomment to see error

  /* 
  The protected setting means:
  - The method (or field) can be accessed by other instances of the same class
  - It is not visible by other code in the current package
  - It is available to subclasses
  */

  // Objects can contain methods
  object StringUtils:
    /**
     * Returns a string that is the same as the input string, but
     * truncated to the specified length.
     */
    def truncate(s: String, length: Int): String = s.take(length)
    /**
      * Returns true if the string contains only letters and numbers.
      */
    def lettersAndNumbersOnly_?(s: String): Boolean =
      s.matches("[a-zA-Z0-9]+")
    /**
     * Returns true if the given string contains any whitespace
     * at all. Assumes that `s` is not null.
     */
    def containsWhitespace(s: String): Boolean =
      s.matches(".*\\s.*")
  end StringUtils

  // Extension methods
  case class Circle(x: Double, y: Double, radius: Double)
  /* 
  There are many situations where you would like to add functionality to closed classes. 
  For example, imagine that you have a Circle class as above, but you can’t change its source code. 
  It could be defined like this in a third-party library
   */

  // When you want to add methods to this class, you can define them as extension methods, like this
  extension (c: Circle)
    def circumference: Double = c.radius * math.Pi * 2
    def diameter: Double = c.radius * 2
    def area: Double = math.Pi * c.radius * c.radius

  // Usage
  val myCircle = Circle(0,0,5) // Create a Circle with a radius of 5

  // Calculate and print the properties of the Circle
  println(s"The circumference of the circle is: ${myCircle.circumference}")
  println(s"The diameter of the circle is: ${myCircle.diameter}")
  println(s"The area of the circle is: ${myCircle.area}")

  
  
  
  
  