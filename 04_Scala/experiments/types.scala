// Based on examples from https://docs.scala-lang.org/scala3/book/types-introduction.html
@main def types() =
  // Inferred types
  val a = 1                             // a: Int
  val b = List(1, 2, 3)                 // b: List[Int]
  val m = Map(1 -> "one", 2 -> "two")   // m: Map[Int, String]

  // Generics
  // here we declare the type parameter A
  //          v
  class Stack[A]:
    private var elements: List[A] = Nil
    //                         ^
    //  Here we refer to the type parameter
    //          v
    def push(x: A): Unit =
      elements = elements.prepended(x)
    def peek: A = elements.head
    def pop(): A =
      val currentTop = peek
      elements = elements.tail
      currentTop

  // This is how you create and use the above example 
  val stack = Stack[Int]
  stack.push(1)
  stack.push(2)
  println(stack.pop())  // prints 2
  println(stack.pop())  // prints 1

  val stack2 = Stack[String]
  stack2.push("Hello")
  stack2.push("World")
  println(stack2.pop())  // prints World
  println(stack2.pop())  // prints Hello

  // Intersection types
  // In the method f in this example, the parameter x is required to be both a 
  // Resettable and a Growable[String].
  trait Resettable:
    def reset(): Unit

  trait Growable[A]:
    def add(a: A): Unit

  def f(x: Resettable & Growable[String]): Unit =
    x.reset()
    x.add("first")

  // Union types
  // In the following example, the help method accepts a parameter named id of the union type 
  // Username | Password, that can be either a Username or a Password:
  type Hash = String
  case class Username(name: String)
  case class Password(hash: Hash)

  def lookupName(name: String): Option[Username] =
    Option(Username(name))
    // Your logic to find the user by name relaces the current code

  def lookupPassword(hash: Hash): Option[Password] =
    Option(Password(hash))
    // Your logic to find the password by hash relaces the current code

  def help(id: Username | Password) =
    val user = id match
      case Username(name) => lookupName(name)
      case Password(hash) => lookupPassword(hash)
      // more code here ..

  // Algebraic Data Types
  // Enumerations
  enum Color:
    case Red, Green, Blue
  
  // they can be parameterized
  enum Color2(val rgb: Int):
    case Red   extends Color2(0xFF0000)
    case Green extends Color2(0x00FF00)
    case Blue  extends Color2(0x0000FF)
  
  println(Color2.Green.rgb) // prints 65280

  // Enums can also have custom definitions
  enum Planet(mass: Double, radius: Double):

    private final val G = 6.67300E-11
    def surfaceGravity = G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity

    case Mercury extends Planet(3.303e+23, 2.4397e6)
    case Venus   extends Planet(4.869e+24, 6.0518e6)
    case Earth   extends Planet(5.976e+24, 6.37814e6)
    // more planets ...

  // Like classes and case classes, you can also define a companion object for an enum
  object Planet:
    def main(args: Array[String]) =
      val earthWeight = args(0).toDouble
      val mass = earthWeight / Earth.surfaceGravity
      for (p <- values)
        println(s"Your weight on $p is ${p.surfaceWeight(mass)}")

  // Algebraic Datatypes (ADTs)
  // This example creates an Option enum with a covariant type parameter T consisting of two cases, 
  // Some and None
  // As with other enumeration uses, ADTs can define additional methods.
  enum Option[+T]:
    case Some(x: T)
    case None

    def isDefined: Boolean = this match
      case None => false
      case Some(_) => true

  object Option:
    def apply[T >: Null](x: T): Option[T] =
      if (x == null) None else Some(x)

  // Hybrid ADT / Enums
  enum Color3(val rgb: Int):
    case Red   extends Color3(0xFF0000)
    case Green extends Color3(0x00FF00)
    case Blue  extends Color3(0x0000FF)
    case Mix(mix: Int) extends Color3(mix)
  
  println(Color3.Mix(234513).rgb)

  // Recursive Enumerations
  enum Nat:
    case Zero
    case Succ(n: Nat)

  // Generalized Algebraic Datatypes (GADTs)
  // Here is an example of a GADT where the type parameter (T) specifies the contents stored in the box
  // and pattern matching on the particular constructor (IntBox or BoolBox) recovers the type information
  enum Box[T](contents: T):
    case IntBox(n: Int) extends Box[Int](n)
    case BoolBox(b: Boolean) extends Box[Boolean](b)

    def extract[T](b: Box[T]): T = b match
      case IntBox(n)  => n + 1
      case BoolBox(b) => !b

  // Variance
  // let's assume the following type definitions
  trait Item { def productNumber: String }
  trait Buyable extends Item { def price: Int }
  trait Book extends Buyable { def isbn: String }

  // also assume the following parameterized types  
  // an example of an invariant type
  trait Pipeline[T]:
    def process(t: T): T

  // an example of a covariant type
  trait Producer[+T]:
    def make: T

  // an example of a contravariant type
  trait Consumer[-T]:
    def take(t: T): Unit

  // Invariant Types  
  def oneOf(
    p1: Pipeline[Buyable],
    p2: Pipeline[Buyable],
    b: Buyable
  ): Buyable =
    val b1 = p1.process(b)
    val b2 = p2.process(b)
    if b1.price < b2.price then b1 else b2

  // Covariant Types
  // def makeTwo(p: Producer[Buyable]): Int =
  //   p.make.price + p.make.price
  // val bookProducer: Producer[Book] = ???
  // makeTwo(bookProducer)


  // Contravariant Types
  trait Function[-A, +B]:
    def apply(a: A): B
  
    val f: Function[Buyable, Buyable] = b => b

    // OK to return a Buyable where a Item is expected
    val g: Function[Buyable, Item] = f

    // OK to provide a Book where a Buyable is expected
    val h: Function[Book, Buyable] = f

  
  // Opaque Types
  object Logarithms:
    //vvvvvv this is the important difference!
    opaque type Logarithm = Double

    object Logarithm:
      def apply(d: Double): Logarithm = math.log(d)

    extension (x: Logarithm)
      def toDouble: Double = math.exp(x)
      def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
      def * (y: Logarithm): Logarithm = x + y

  // Outside of the module the type Logarithm is completely encapsulated, or “opaque.” 
  // To users of Logarithm it is not possible to discover that Logarithm is actually implemented 
  // as a Double    
  import Logarithms.*
  val log2 = Logarithm(2.0)
  val log3 = Logarithm(3.0)
  println((log2 * log3).toDouble) // prints 6.0
  println((log2 + log3).toDouble) // prints 4.999...

  // val d: Double = log2 // ERROR: Found Logarithm required Double - uncomment to see error

  // Structural types help in situations where you’d like to support simple dot notation in dynamic 
  // contexts without losing the advantages of static typing. They allow developers to use dot 
  // notation and configure how fields and methods should be resolved.

  class Record(elems: (String, Any)*) extends Selectable:
    private val fields = elems.toMap
    def selectDynamic(name: String): Any = fields(name)

  type Person = Record {
    val name: String
    val age: Int
  }

  val person = Record(
    "name" -> "Emma",
    "age" -> 42
  ).asInstanceOf[Person]

  println(s"${person.name} is ${person.age} years old.")

  // Another example
  type Book2 = Record {
    val title: String
    val author: String
    val year: Int
    val rating: Double
  }

  val book = Record(
    "title" -> "The Catcher in the Rye",
    "author" -> "J. D. Salinger",
    "year" -> 1951,
    "rating" -> 4.5
  ).asInstanceOf[Book2]

  println(book.title)