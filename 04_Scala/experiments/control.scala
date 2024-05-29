// Based on examples from https://docs.scala-lang.org/scala3/book/taste-control-structures.html
//                        https://docs.scala-lang.org/scala3/book/control-structures.html
@main def control() =
  val x = 1
  // if one liner
  if x == 1 then println(x)
  
  // if multi liner
  if x == 1 then
    println("x is 1, as you can see below:")
    println(x)

  // an optional end if
  if x == 1 then
    println("x is 1, as you can see:")
    println(x)
  end if

  // if / else
  if x == 1 then
    println("x is 1, as you can see below:")
    println(x)
  else
    println("x was not 1")
  
  // if / else if / else
  if x < 0 then
    println("negative")
  else if x == 0 then
    println("zero")
  else
    println("positive")

  
  // if / else expression 
  val a = 1
  val b = 2
  val y = if a < b then a else b    // no need for ternery operator in Scala
  println(y) 

  // because then can return a value they can also be used as follows in a body of a method
  def compare(a: Int, b: Int): Int =
    if a < b then
      -1
    else if a == b then
      0
    else
      1
  
  val u = 1
  val v = 2
  println(compare(u,v))
  
  // for loops and expressions
  // a simple for loop used as an iterator over a list (the i <- ints parts is called a generator )
  val ints = List(1, 2, 3, 4, 5)
  for i <- ints do println(i)

  // multiline for loop 
  for i <- ints
  do
    val x = i * 2
    println(s"i = $i, x = $x")

  // a for loop with multiple generators
  for
    i <- 1 to 2
    j <- 'a' to 'b'
    k <- 1 to 10 by 5
  do
    println(s"i = $i, j = $j, k = $k")


  // for loops can also contain if statements, which are known as guards:
  // Guards in for loops
  for
    i <- ints
    if i > 2
  do
    println(i)

  // multiple guards
  for
    i <- 1 to 10
    if i > 3
    if i < 6
    if i % 2 == 0
  do
    println(i)

  // multiple generators and guards
  for
    i <- 1 to 3
    j <- 'a' to 'c'
    if i > 1
    if j == 'b'
  do
    println(s"i = $i, j = $j")   // prints: "i = 2, j = b" and "i = 3, j = b"

  // using for with maps
  val province = Map(
    "NS" -> "Nova Scotia",
    "NB" -> "New Brunswick", 
    "PEI" -> "Prince Edward Island",
    "NL" -> "Newfoundland and Labrador"
  )
  for (abbrev, fullName) <- province do println(s"$abbrev: $fullName")
  

  // for expressions using yield
  val doublesA = for i <- ints yield i * 2
  println(doublesA)

  // note this structure can take several forms all of which would work
  val doublesB = for (i <- ints) yield i * 2
  val doublesC = for (i <- ints) yield (i * 2)
  val doublesD = for { i <- ints } yield (i * 2)

  // also notice that using for / yield expressions can give the same result using map
  // for / yield version
  val listA =
    for i <- 10 to 12
    yield i * 2
  println(listA)
  // map version
  val listB = (10 to 12).map(i => i * 2)
  println(listB)

  // you can use for to itterate over a list in the context of an expression
  val names = List("chris", "ed", "maurice")
  val capNames = for name <- names yield name.capitalize
  println(capNames)

  // here is a another example of an expression of this type
  val fruits = List("apple", "banana", "lime", "orange")
  val fruitLengths = for
    f <- fruits
    if f.length > 4
  yield
    // multiple lines of code could go here
    f.length
  println(fruitLengths)  

  // for expressions can be used any time you need to traverse all the elements in a collection 
  // and apply an algorithm to those elements to create a new list.
  // An example that shows how to use a block of code after the yield
  val namesList = List("_socket", "_yowlie", "_dave")

  val capNamesList = for name <- namesList yield
    val nameWithoutUnderscore = name.drop(1)
    val capName = nameWithoutUnderscore.capitalize
    capName

  println(capNamesList)

  // for expression as the body of a method
  def between3and10(xs: List[Int]): List[Int] =
    for
      x <- xs
      if x >= 3
      if x <= 10
    yield x

  println(between3and10(List(1, 3, 7, 11)))

  // match expressions with _ as a catch all
  val i = 1
  i match
    case 1 => println("one")
    case 2 => println("two")
    case _ => println("other")
  
  // you can also a match result to a variable 
  val result = i match
    case 1 => "one"
    case 2 => "two"
    case _ => "other"
  println(result) 

  // you can do the following to access the value that matched to the catch all 
  // what can be any valid variable name
  val amount = 7
  amount match
    case 0 => println("1")
    case 1 => println("2")
    case what => println(s"You gave me this amount: $what")

  // you can have multiple possible matches on one line
  val evenOrOdd = i match
    case 1 | 3 | 5 | 7 | 9 => println("odd")
    case 2 | 4 | 6 | 8 | 10 => println("even")
    case _ => println("some other number")

  // match can also be used with other data types
  case class Person(name: String)
  val p = Person("Fred")
  p match
    case Person(name) if name == "Fred" =>
      println(s"$name says, Yubba dubba doo")
    case Person(name) if name == "Bam Bam" =>
      println(s"$name says, Bam bam!")
    case _ => println("Watch the Flintstones!")

  // getClassAsString is a method that takes a single argument of any type.
  def getClassAsString(x: Matchable): String = x match
    case s: String => s"'$s' is a String"
    case i: Int => "Int"
    case d: Double => "Double"
    case l: List[?] => "List"
    case _ => "Unknown"

  // examples
  println(getClassAsString(1))               // Int
  println(getClassAsString("hello"))         // 'hello' is a String
  println(getClassAsString(List(1, 2, 3)))   // List

  // using if guards in case clauses
  i match
    case 1 => println("one, is the loneliest number")
    case x if x == 2 || x == 3 => println("two's company, three's a crowd")
    case x if x > 3 => println("4+, party time!")
    case _ => println("I guess your number is zero or less")
  
  // using if guards with ranges in case clauses
  i match
    case a if 0 to 9 contains a => println(s"0-9 range: $a")
    case b if 10 to 19 contains b => println(s"10-19 range: $b")
    case c if 20 to 29 contains c => println(s"20-29 range: $c")
    case _ => println("Hmmm...")

  // Using a match expression as the body of a method
  def isTruthy(a: Matchable) = a match
    case 0 | "" | false => false
    case _              => true
  
  println(isTruthy(0)) 
  println(isTruthy(false))  
  println(isTruthy(""))     
  println(isTruthy(1))      
  println(isTruthy(" "))    
  println(isTruthy(2F))     


  /* 
  Match expressions support many different types of patterns, these include
  - Constant patterns (such as case 3 => )
  - Sequence patterns (such as case List(els : _*) =>)
  - Tuple patterns (such as case (x, y) =>)
  - Constructor pattern (such as case Person(first, last) =>)
  - Type test patterns (such as case p: Person =>)
  */

  // The following example method from the Scala 3 Book shows these as examples in a method
  // The method takes an input parameter of type Matchable and returns a String
  case class Persona(first: String, last: String)
  case class Dog(name: String)

  def pattern(x: Matchable): String = x match

    // constant patterns
    case 0 => "zero"
    case true => "true"
    case "hello" => "you said 'hello'"
    case Nil => "an empty List"

    // sequence patterns
    case List(0, _, _) => "a 3-element list with 0 as the first element"
    case List(1, _*) => "list, starts with 1, has any number of elements"
    case Vector(1, _*) => "vector, starts w/ 1, has any number of elements"

    // tuple patterns
    case (a, b) => s"got $a and $b"
    case (a, b, c) => s"got $a, $b, and $c"

    // constructor patterns
    case Persona(first, "Alexander") => s"Alexander, first name = $first"
    case Dog("Zeus") => "found a dog named Zeus"

    // type test patterns
    case s: String => s"got a string: $s"
    case i: Int => s"got an int: $i"
    case f: Float => s"got a float: $f"
    case a: Array[Int] => s"array of int: ${a.mkString(",")}"
    case as: Array[String] => s"string array: ${as.mkString(",")}"
    case d: Dog => s"dog: ${d.name}"
    case list: List[?] => s"got a List: $list"
    case m: Map[?, ?] => m.toString

    // the default wildcard pattern
    case _ => "Unknown"


  // try/catch/finally
//   try
//     writeTextToFile(text)
//   catch
//     case ioe: IOException => println("Got an IOException.")
//     case nfe: NumberFormatException => println("Got a NumberFormatException.")
//   finally
//     println("Clean up your resources here.")

  // single line while loop
  var xx = 5
  val f = (i: Int) => i - 1
  println(xx)
  while xx >= 0 do xx = f(xx)
  println(xx)

  // multi line while loop
  var x1 = 1
  while (x1 < 3)
    println(x1)
    x1 += 1

