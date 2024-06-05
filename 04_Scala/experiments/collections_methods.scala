// Based on examples from https://docs.scala-lang.org/scala3/book/collections-methods.html
@main def collections_methods() =
  // Common methods
  val a = List(10, 20, 30, 40, 10)      // List(10, 20, 30, 40, 10)

  println(a.distinct)                            // List(10, 20, 30, 40)
  println(a.drop(2))                             // List(30, 40, 10)
  println(a.dropRight(2))                        // List(10, 20, 30)
  println(a.head)                                // 10
  println(a.headOption)                          // Some(10)
  println(a.init)                                // List(10, 20, 30, 40)
  println(a.intersect(List(19,20,21)))           // List(20)
  println(a.last)                                // 10
  println(a.lastOption)                          // Some(10)
  println(a.slice(2,4))                          // List(30, 40)
  println(a.tail)                                // List(20, 30, 40, 10)
  println(a.take(3))                             // List(10, 20, 30)
  println(a.takeRight(2))                        // List(40, 10)

  // Higher-order functions and lambdas
  // these functions are all equivalent and return
  // the same data: List(10, 20, 10)
  println(a.filter((i: Int) => i < 25))   // 1. most explicit form
  println(a.filter((i) => i < 25))        // 2. `Int` is not required
  println(a.filter(i => i < 25))          // 3. the parens are not required
  println(a.filter(_ < 25))               // 4. `i` is not required
  println(a.dropWhile(_ < 25))            // List(30, 40, 10)
  println(a.filter(_ > 100))              // List()
  println(a.filterNot(_ < 25))            // List(30, 40)
  println(a.find(_ > 20))                 // Some(30)
  println(a.takeWhile(_ < 30))            // List(10, 20)

  def double(i: Int) = i * 2
  // these all return `List(20, 40, 60, 80, 20)`
  println(a.map(i => double(i)))
  println(a.map(double(_)))
  println(a.map(double))

  // yields `List(100, 200)`
  println(a.filter(_ < 40)
           .takeWhile(_ < 30)
           .map(_ * 10))

  // Collections - methods
  // starting data
  val oneToTen = (1 to 10).toList
  val names = List("ted", "pink", "vrog", "erik")

  // map 
  val doubles = oneToTen.map(_ * 2)
  val capNames = names.map(_.capitalize)
  val nameLengthsMap = names.map(s => (s, s.length)).toMap
  val isLessThanFive = oneToTen.map(_ < 5)

  // filter
  val lessThanFive = oneToTen.filter(_ < 5)
  val evens = oneToTen.filter(_ % 2 == 0)
  val shortNames = names.filter(_.length <= 4)

  // chaining methods
  println(oneToTen.filter(_ < 4).map(_ * 10))

  // foreach
  names.foreach(println)

  // head - remember empty lists will throw an exception (headOption does not)
  println(oneToTen.head)   // 1
  println(names.head)      // ted

  // tail - remember empty lists will throw an exception (tailOption does not)
  println(oneToTen.tail)   // List(2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(names.tail)      // List("pink", "vrog", "erik")

  // matching
  val x :: xs = names
  println(x)
  println(xs)

  // pattern matching can be used to write recursive methods - like this for sum
  def sum(list: List[Int]): Int = list match
    case Nil => 0
    case x :: xs => x + sum(xs)
  println(sum(a))
  
  // take, takeRight, takeWhile
  println(oneToTen.take(1))                   // List(1)
  println(oneToTen.take(2))                   // List(1, 2)
  println(oneToTen.takeRight(1))              // List(10)
  println(oneToTen.takeRight(2))              // List(9, 10)
  println(oneToTen.take(Int.MaxValue))        // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(oneToTen.takeRight(Int.MaxValue))   // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(oneToTen.take(0))                   // List()
  println(oneToTen.takeRight(0))              // List()
  println(oneToTen.takeWhile(_ < 5))          // List(1, 2, 3, 4)
  println(names.takeWhile(_.length < 5))      // List(adam)

  // drop, dropRight, dropWhile
  println(oneToTen.drop(1))                   // List(2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(oneToTen.drop(5))                   // List(6, 7, 8, 9, 10)

  println(oneToTen.dropRight(8))              // List(1, 2)
  println(oneToTen.dropRight(7))              // List(1, 2, 3)
  println(oneToTen.drop(Int.MaxValue))        // List()
  println(oneToTen.dropRight(Int.MaxValue))   // List()
  println(oneToTen.drop(0))                   // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(oneToTen.dropRight(0))              // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(oneToTen.dropWhile(_ < 5))          // List(5, 6, 7, 8, 9, 10)
  println(names.dropWhile(_ != "chris"))      // List(chris, david)

  // reduce
  // using with the following methods and list to start with...
  def add(x: Int, y: Int): Int =
    val theSum = x + y
    println(s"received $x and $y, their sum is $theSum")
    theSum
  val b = List(1,2,3,4)

  // here is some interesting applications of reduce
  println(b.reduce(add))     // sum
  println(b.reduce(_ + _))   // sum
  println(b.reduce(_ * _))   // product