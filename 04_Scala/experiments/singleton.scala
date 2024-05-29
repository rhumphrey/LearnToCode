// Based on examples from https://docs.scala-lang.org/scala3/book/taste-objects.html
@main def singleton() =
  // In Scala, the object keyword creates a Singleton object. 
  // An object defines a class that has exactly one instance.
  // Objects have several uses:
  // - They are used to create collections of utility methods.
  // - A companion object is an object that has the same name as the class it shares a file with. In this situation, that class is also called a companion class.
  // - They're used to implement traits to create modules.

  // “Utility” methods
  object StringUtils:
    def isNullOrEmpty(s: String): Boolean = s == null || s.trim.isEmpty
    def leftTrim(s: String): String = s.replaceAll("^\\s+", "")
    def rightTrim(s: String): String = s.replaceAll("\\s+$", "")

  // StringUtils is a singleton, so its methods can be called directly on the object
  val x = StringUtils.isNullOrEmpty("")    // true
  println(x)
  val y = StringUtils.isNullOrEmpty("a")   // false
  println(y)

  // Companion objects
  import scala.math.*
  
  class Circle(radius: Double):
    import Circle.*
    def area: Double = calculateArea(radius)

  object Circle:
    private def calculateArea(radius: Double): Double =
      Pi * pow(radius, 2.0)

  val circle1 = Circle(5.0)
  println(circle1.area)                 // Double = 78.53981633974483

  // Creating modules from traits
  // Objects can also be used to implement traits to create modules
  trait AddService:
    def add(a: Int, b: Int) = a + b

  trait MultiplyService:
    def multiply(a: Int, b: Int) = a * b

  // Implement those traits as a concrete object
  object MathService extends AddService, MultiplyService

  // Use the object
  import MathService.*
  val xx = 2
  val yy = 3
  println(add(xx,yy))        
  println(multiply(xx,yy))  