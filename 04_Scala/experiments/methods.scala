// Based on examples from https://docs.scala-lang.org/scala3/book/taste-methods.html
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
  You don’t have to declare a method’s return type, so you can write those methods like this
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