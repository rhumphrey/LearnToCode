@main def control() =
  
  // if/else
  val x = 1
  if x < 0 then
    println("negative")
  else if x == 0 then
    println("zero")
  else
    println("positive")
  
  val a = 1
  val b = 2
  val y = if a < b then a else b
  println(a) 
  
  // for loops and expressions
  val ints = List(1, 2, 3, 4, 5)
  for i <- ints do println(i)

  // Guards in for loops
  for
    i <- ints
    if i > 2
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

  // for expressions
  val doublesA = for i <- ints yield i * 2
  println(doublesA)

  // note this structure can take several forms all of witch would work
  val doublesB = for (i <- ints) yield i * 2
  val doublesC = for (i <- ints) yield (i * 2)
  val doublesD = for { i <- ints } yield (i * 2)

  // you can use for to itterate over a list
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

  // match expressions
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

  // it can also be used with other data types
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

