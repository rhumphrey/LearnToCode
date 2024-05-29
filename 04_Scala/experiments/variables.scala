// Based on examples from https://docs.scala-lang.org/scala3/book/taste-vars-data-types.html
import scala.reflect.ManifestFactory.NothingManifest
@main def variables() =
  
  // Mutability
  // immutable if you use val - can't be reassigned - You’ll cause a compiler error if you try to reassign one
  val valA = 0
  // mutable if you use var - can be reassigned
  var varA = 1

  // Declaring variable types
  val exX: Int = 1   // explicit
  val imX = 1        // implicit; the compiler infers the type - type inference
  
  // examples using inference
  val imString = "a string" // infers that s is a string
  val imNums = List(1, 2, 3) // infers that nums is a list

  // examples of the explicit version (more verbose)
  val exString: String = "a string"
  val exNums: List[Int] = List(1, 2, 3)

  // Built-in data types (declaring explicitly)
  val exB: Byte = 1
  val exI: Int = 1
  val exL: Long = 1
  val exS: Short = 1
  val exD: Double = 2.0
  val exF: Float = 3.0

  //  Built-in data types (declaring implicitly - with a bit of a nudge in some cases)
  val imI = 123      // defaults to Int
  val imD = 1.0      // defaults to Double you can also use - val im_d = 2.2D
  val imL = 1_000L   // val imL: Long = 1000
  val imF = 3.3F     // val imF: Float = 3.3

  // Here a a couple examples of hexadecimal notation notice the use of L in the second
  val imHexIa = 0xACE      // val imHexIa: Int = 2766
  val imHexIb = 0xfd_3aL   // val imHexIb: Long = 64826

  // There are also several ways to format floating point numbers
  val imQ = .25      // val imQ: Double = 0.25
  val imR = 2.5e-1   // val imR: Double = 0.25
  val imS = .0025e2F // val imS: Float = 0.25


  // There is also BigInt and BigDecimal, when you need really large numbers
  var bI = BigInt(1_234_567_890_987_654_321L)
  var bD = BigDecimal(123_456.789)

  // Scala also has a Char data type
  val imC = 'a'         // Char

  // Strings
  // String interpolation
  val firstName = "Teddy"
  val middleInitial = 'J'
  val lastName = "McTedface"
  println(s"Name: $firstName $middleInitial $lastName")   // "Name: Teddy J McTedFace"

  // Embedding expresions
  println(s"2 + 2 = ${2 + 2}")   // prints "2 + 2 = 4"
  val x = -1
  println(s"x.abs = ${x.abs}")   // prints "x.abs = 1"

  val quote = """The essence of Scala:
                |Fusion of functional and object-oriented
                |programming in a typed setting.""".stripMargin
  println(quote)

  // A Further look at types
  // Based on examples from https://docs.scala-lang.org/scala3/book/first-look-at-types.html

  val list: List[Any] = List(
    "a string",
    732,  // an integer
    'c',  // a character
    '\'', // a character with a backslash escape
    true, // a boolean value
    () => "an anonymous function returning a string"
  )

  list.foreach(element => println(element))
  list.foreach(element => println(element.getClass.getSimpleName)) // print the type of each element

  // Type casting
  val b: Byte = 127
  val i: Int = b  // 127
  println(b)
  println(i)

  val face: Char = '☺'
  val number: Int = face  // 9786
  println(face)
  println(number)

  // You can only cast to a type where there is no loss of information. 
  // Otherwise, you need to be explicit about the cast
  val theX: Long = 987654321
  val theY: Float = theX.toFloat  // 9.8765434E8 (note that `.toFloat` is required because the cast results in precision loss)
  // val theZ: Long = theY        // Remove the first // to see the error

  // Null and Nothing in Scala
  /* 
  1. Nothing:
   - Definition: `Nothing` is the absolute "no value" type in Scala. 
                  It doesn't have any methods or values.
   - Usage:
     - It serves as the root of the entire Scala type system.
     - We can use `Nothing` in place of any Scala type, both reference types and value types.
     - Together with the `Null` type, it sits at the bottom of the type hierarchy.
   - Common Use Case:
     - Signaling non-termination: For example, when an expression throws an exception, 
       leads to program exit, or results in an infinite loop.
     - It represents an expression that does not evaluate to a value or a method that 
       does not return normally.
  2. Null:
   - Definition: `Null` is the type of the null reference. It extends all reference types, 
                 including custom classes and traits.
   - Value: The keyword literal `null` represents the single value of type `Null`.
   - Usage:
     - Historically, `null` has been used to represent an absent value.
     - However, its usage is considered bad practice in Scala.
     - It should mainly be used for interoperability with other JVM languages.
     - An opt-in compiler option can address some of the caveats related to its usage.

  Important points:
  - Avoid using `null` while initializing variables if there's an empty value of the 
    variable's type available (e.g., use `Nil` for lists).
  - Follow Scala best practices by wrapping return values in the `Option` type instead of 
    returning `null`. This helps prevent dreaded `NullPointerExceptions`.
  */
  
  // while it is possible to create a null variable (and you should avoid it) it is not possible
  // to directly create a variable of type Nothing. It typically arises in specific situations, 
  // such as signaling non-termination (e.g., exceptions, program exit, infinite loops).

  val myNullVariable: Null = null
  println(myNullVariable)
