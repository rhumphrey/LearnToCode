// Based on examples from https://docs.scala-lang.org/scala3/book/taste-collections.html
@main def collections() =
  // Creating lists
  // 
  val a = List(1, 2, 3)           // a: List[Int] = List(1, 2, 3)

  // Range methods
  val b = (1 to 5).toList         // b: List[Int] = List(1, 2, 3, 4, 5)
  val c = (1 to 10 by 2).toList   // c: List[Int] = List(1, 3, 5, 7, 9)
  val e = (1 until 5).toList      // e: List[Int] = List(1, 2, 3, 4)
  val f = List.range(1, 5)        // f: List[Int] = List(1, 2, 3, 4)
  val g = List.range(1, 10, 3)    // g: List[Int] = List(1, 4, 7)
  println(s"$a\n$b\n$c\n$e\n$e\n$f\n$g")

  
  // List methods
  val aList = List(10, 20, 30, 40, 10)      // List(10, 20, 30, 40, 10)

  println(aList.drop(2))                             // List(30, 40, 10)
  println(aList.dropWhile(_ < 25))                   // List(30, 40, 10)
  println(aList.filter(_ < 25))                      // List(10, 20, 10)
  println(aList.slice(2,4))                          // List(30, 40)
  println(aList.head)                                // 10
  println(aList.tail)                                // List(20, 30, 40, 10)
  println(aList.take(3))                             // List(10, 20, 30)
  println(aList.takeWhile(_ < 30))                   // List(10, 20)

  // flatten a list of lists
  val aListofLists = List(List(1,2), List(3,4))
  println(aListofLists.flatten)                      // List(1, 2, 3, 4)

  // map, flatMap
  val nums = List("one", "two")
  println(nums.map(_.toUpperCase))                   // List("ONE", "TWO")
  println(nums.flatMap(_.toUpperCase))               // List('O', 'N', 'E', 'T', 'W', 'O')

  val firstTen = (1 to 10).toList            // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  firstTen.reduceLeft(_ + _)                 // 55
  firstTen.foldLeft(100)(_ + _)              // 155 (100 is a “seed” value)

  // Tuples
  // Collection of different types in the same container.
  case class Person(name: String)
  // creating a tuple that contains an Int, a String, and a custom Person value
  val t = (11, "eleven", Person("Eleven"))

  // to acces the values
  println(t(0))   
  println(t(1))   
  println(t(2))   

  // you can also extract this way
  val (num, str, person) = t
  println(num)
  println(str)
  println(person)

  