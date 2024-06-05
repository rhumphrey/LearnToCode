// Based on examples from https://docs.scala-lang.org/scala3/book/fp-intro.html
@main def functional() =
  // immutable values
  // So, with immutable collections you don’t mutate an existing collection; 
  // instead, you apply a function to an existing collection to create a new collection.
  val originalList = List("jane", "jon", "mary", "joe")
  val newList = originalList.filter(_.startsWith("j"))
         .map(_.capitalize)

  // For clases You typically create case classes, whose constructor parameters are val by default
  case class Person(firstName: String, lastName: String)
  val reginald = Person("Reginald", "Dwight")
  // then use copy if you need to make changes
  val elton = reginald.copy(
    firstName = "Elton",   // update the first name
    lastName = "John"      // update the last name
  )
  
  /* 
  Pure Functions
  Another feature that Scala offers to help you write functional code is the ability to write pure functions. A pure function can be defined like this:

  A function f is pure if, given the same input x, it always returns the same output f(x)
  The function’s output depends only on its input variables and its implementation
  It only computes the output and does not modify the world around it
  This implies:
    It doesn’t modify its input parameters
    It doesn’t mutate any hidden state
    It doesn’t have any “back doors”: It doesn’t read data from the outside world 
    (including the console, web services, databases, files, etc.), or write data to the outside world

    "A pure function is a function that depends only on its declared inputs and its implementation to produce its output. 
    It only computes its output and does not depend on or modify the outside world."
  */

  // You can create functions as values
  val nums = (1 to 10).toList

  val doubles = nums.map(_ * 2)           // double each value
  val lessThanFive = nums.filter(_ < 5)   // List(1,2,3,4)
  println(doubles)
  println(lessThanFive)

  // you can also do similar things with methods
  // two methods
  def double(i: Int): Int = i * 2
  def underFive(i: Int): Boolean = i < 5

  // pass those methods into filter and map
  val underFivesDoubled = nums.filter(underFive).map(double)
  println(underFivesDoubled)

  // you can go down an anonomous function route and give it a name by assigning to a variable
  val doubleIt = (i: Int) => i * 2
  println(doubleIt(2))

  // more fun with functions and passing functions as parameters to other functions
  println(List("bob", "joe").map(_.toUpperCase))   // List(BOB, JOE)
  println(List("bob", "joe").map(_.capitalize))    // List(Bob, Joe)
  println(List("plum", "banana").map(_.length))    // List(4, 6)

  val fruits = List("apple", "pear")
  println(fruits.map(_.toUpperCase))               // List(APPLE, PEAR)
  println(fruits.flatMap(_.toUpperCase))           // List(A, P, P, L, E, P, E, A, R)

  val nums2 = List(5, 1, 3, 11, 7)
  println(nums2.map(_ * 2))                        // List(10, 2, 6, 22, 14)
  println(nums2.filter(_ > 3))                     // List(5, 11, 7)
  println(nums2.takeWhile(_ < 6))                  // List(5, 1, 3)
  println(nums2.sortWith(_ < _))                   // List(1, 3, 5, 7, 11)
  println(nums2.sortWith(_ > _))                   // List(11, 7, 5, 3, 1)

  println(nums2.takeWhile(_ < 6).sortWith(_ < _))  // List(1, 3, 5)

  // Functional error handling
  /* 
  The Some and None classes are subclasses of Option, so the solution works like this:
  - You declare that makeInt returns an Option type
  - If makeInt receives a string it can convert to an Int, the answer is wrapped inside a Some
  - If makeInt receives a string it can’t convert, it returns a None
  */
  def makeInt(s: String): Option[Int] =
    try
      Some(Integer.parseInt(s.trim))
    catch
      case e: Exception => None

  val a = makeInt("1")     // Some(1)
  val b = makeInt("one")   // None
  println(a)
  println(b)
  
  // Using makeInt
  // with match
  val x = "Hello"
  makeInt(x) match
    case Some(i) => println(i)
    case None => println("That didn't work.")
  
  // with for
  val stringA = "1"
  val stringB = "2"
  val stringC = "Hello"

  val y = for 
    a <- makeInt(stringA)
    b <- makeInt(stringB)
    c <- makeInt(stringC)
  yield
    a + b + c
  
  println(y)

  // Using Option to replace none
  class Address(
    var street1: String,
    var street2: Option[String],   // an optional value
    var city: String, 
    var state: String, 
    var zip: String
  )

  // Then usage is more accurate
  val santa1 = Address(
    "1 Main Street",
    None,           // 'street2' has no value
    "North Pole",
    "Alaska",
    "99705"
  )

  val santa2 = Address(
    "123 Main Street",
    Some("Apt. 2B"),
    "Talkeetna",
    "Alaska",
    "99676"
  )