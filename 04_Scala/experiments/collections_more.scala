// Based on examples from https://docs.scala-lang.org/scala3/book/collections-classes.html
@main def collections_more() =
  // Lists - The List type is a linear, immutable sequence
  // cresting lists
  val ints = List(1, 2, 3)
  val names = List("Joel", "Chris", "Ed")
  // another way to construct a List
  val namesAgain = "Joel" :: "Chris" :: "Ed" :: Nil
  // you can also declare the list type
  val intsMore: List[Int] = List(1, 2, 3)
  val namesMore: List[String] = List("Joel", "Chris", "Ed")
  // for mixed types in a list consider the following
  val things: List[String | Int | Double] = List(1, "two", 3.0) // with union types
  val thingsAny: List[Any] = List(1, "two", 3.0)                // with any

  // Because List is immutable, you can’t add new elements to it. 
  // Instead, you create a new list by prepending or appending elements to an existing List.
  val a = List(1, 2, 3)
  val b = 0 :: a              // List(0, 1, 2, 3)
  val c = List(-1, 0) ::: a   // List(-1, 0, 1, 2, 3)
  val d = a :+ 4              // List(1, 2, 3, 4)
  val e = a ::: List(4, 5)   // List(1, 2, 3, 4, 5)

  // loop over lists
  val namesList = List("Socket", "Yowlie", "Dave")
  for name <- namesList do println(name)

  // A Vector is an indexed, immutable sequence.
  // creating vectors
  val nums = Vector(1, 2, 3, 4, 5)

  val strings = Vector("one", "two")

  case class Person(name: String)
  val people = Vector(
    Person("Bert"),
    Person("Ernie"),
    Person("Grover")
  )

  // Again, like lists its immutable
  // Here is how you append
  val a1 = Vector(1,2,3)          // Vector(1, 2, 3)
  val b1 = a1 :+ 4                // Vector(1, 2, 3, 4)
  val c1 = a1 ++ Vector(4, 5)     // Vector(1, 2, 3, 4, 5)

  // Here is how you prepend
  val a2 = Vector(1,2,3)          // Vector(1, 2, 3)
  val b2 = 0 +: a2                // Vector(0, 1, 2, 3)
  val c2 = Vector(-1, 0) ++: a2   // Vector(-1, 0, 1, 2, 3)

  // looping over vectors
  val names1 = Vector("Socket", "Yowlie", "Dave")
  for name <- names1 do println(name)

  // Use ArrayBuffer when you need a general-purpose, mutable indexed sequence
  // If you need to start with an empty ArrayBuffer, just specify its type
  import scala.collection.mutable.ArrayBuffer
  var strings1 = ArrayBuffer[String]()
  var ints1 = ArrayBuffer[Int]()
  var people1 = ArrayBuffer[Person]()

  // If you know the approximate size your ArrayBuffer eventually needs to be
  // ready to hold 100,000 ints
  val buf = new ArrayBuffer[Int](100_000)

  // To create a new ArrayBuffer with initial elements
  val nums2 = ArrayBuffer(1, 2, 3)
  val people2 = ArrayBuffer(
    Person("Bert"),
    Person("Ernie"),
    Person("Grover")
  )
  
  // Adding elements to an ArrayBuffer
  val nums3 = ArrayBuffer(1, 2, 3)   // ArrayBuffer(1, 2, 3)
  nums3 += 4                         // ArrayBuffer(1, 2, 3, 4)
  nums3 ++= List(5, 6)               // ArrayBuffer(1, 2, 3, 4, 5, 6)
  // You can also use methods such as append, appendAll, insert, insertAll, prepend, and prependAll

  // Removing elements from an ArrayBuffer
  val a3 = ArrayBuffer.range('a', 'h')   // ArrayBuffer(a, b, c, d, e, f, g)
  a3 -= 'a'                              // ArrayBuffer(b, c, d, e, f, g)
  a3 --= Seq('b', 'c')                   // ArrayBuffer(d, e, f, g)
  a3 --= Set('d', 'e')                   // ArrayBuffer(f, g)

  // Updating ArrayBuffer element
  val a4 = ArrayBuffer.range(1,5)        // ArrayBuffer(1, 2, 3, 4)
  a4(2) = 50                             // ArrayBuffer(1, 2, 50, 4)
  a4.update(0, 10)                       // ArrayBuffer(10, 2, 50, 4)

  // Maps
  // Creating an immutable Map
  val states = Map(
    "AK" -> "Alaska",
    "AL" -> "Alabama",
    "AZ" -> "Arizona"
  )

  // Once you have a Map you can traverse its elements in a for loop like this:
  for (k, v) <- states do println(s"key: $k, value: $v")

  // Accessing Map elements
  val ak = states("AK")   // ak: String = Alaska
  val al = states("AL")   // al: String = Alabama

  // Adding elements to a Map
  val a5 = Map(1 -> "one")     // a: Map(1 -> one)
  val b5 = a5 + (2 -> "two")   // b: Map(1 -> one, 2 -> two)
  val c5 = b5 ++ Seq(
    3 -> "three",
    4 -> "four"
  )                            // c: Map(1 -> one, 2 -> two, 3 -> three, 4 -> four)
  println(c5)

  // Removing elements from a map
  val a6 = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four"
  )

  val b6 = a6 - 4       // b: Map(1 -> one, 2 -> two, 3 -> three)
  val c6 = a6 - 4 - 3   // c: Map(1 -> one, 2 -> two)
  println(c6)

  // Updating Map elements
  val a7 = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three"
  )

  val b7 = a7.updated(3, "THREE!")   // b: Map(1 -> one, 2 -> two, 3 -> THREE!)
  val c7 = a7 + (2 -> "TWO...")      // c: Map(1 -> one, 2 -> TWO..., 3 -> three)

  // Traversing a Map
  val statesAgain = Map(
    "AK" -> "Alaska",
    "AL" -> "Alabama",
    "AZ" -> "Arizona"
  )

  // Sets -  A Set is an iterable collection with no duplicate elements
  // Creating a Set
  // Empty
  val nums4 = Set[Int]()
  val letters4 = Set[Char]()
  // Initial data
  val nums5 = Set(1, 2, 3, 3, 3)           // Set(1, 2, 3)
  val letters5 = Set('a', 'b', 'c', 'c')   // Set('a', 'b', 'c')
  
  // Adding elements
  val a8 = Set(1, 2)                  // Set(1, 2)
  val b8 = a8 + 3                     // Set(1, 2, 3)
  val c8 = b8 ++ Seq(4, 1, 5, 5)      // HashSet(5, 1, 2, 3, 4)

  val a9 = Set(1, 2, 3, 4, 5)    // HashSet(5, 1, 2, 3, 4)
  val b9 = a9 - 5                // HashSet(1, 2, 3, 4)
  val c9 = b9 -- Seq(3, 4)       // HashSet(1, 2)

  // Range
  // Range is often used to populate data structures and to iterate over for loops
  println(1 to 5)         // Range(1, 2, 3, 4, 5)
  println(1 until 5)      // Range(1, 2, 3, 4)
  println(1 to 10 by 2)   // Range(1, 3, 5, 7, 9)
  println('a' to 'c')     // NumericRange(a, b, c)
  
  // You can use ranges to populate collections:
  val x = (1 to 5).toList     // List(1, 2, 3, 4, 5)
  val x1 = (1 to 5).toBuffer   // ArrayBuffer(1, 2, 3, 4, 5)

  // Also in for loops
  for i <- 1 to 3 do println(i)

  // And as a method on collections
  println(Vector.range(1, 5))       // Vector(1, 2, 3, 4)
  println(List.range(1, 10, 2))     // List(1, 3, 5, 7, 9)
  println(Set.range(1, 10) )        // HashSet(5, 1, 6, 9, 2, 7, 3, 8, 4)

  // Ranges are also useful for generating test collections
  val evens = (0 to 10 by 2).toList     // List(0, 2, 4, 6, 8, 10)
  val odds = (1 to 10 by 2).toList      // List(1, 3, 5, 7, 9)
  val doubles = (1 to 5).map(_ * 2.0)   // Vector(2.0, 4.0, 6.0, 8.0, 10.0)

  // create a Map
  val map = (1 to 3).map(e => (e,s"$e")).toMap  // map: Map[Int, String] = Map(1 -> "1", 2 -> "2", 3 -> "3")

  println(evens)
  println(odds)
  println(doubles)
  println(map)


