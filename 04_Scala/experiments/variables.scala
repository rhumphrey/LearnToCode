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
  val imL = 1_000L   // val x: Long = 1000
  val imF = 3.3F     // val z: Float = 3.3

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