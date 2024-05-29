// Based on examples from https://docs.scala-lang.org/scala3/book/taste-functions.html
@main def functions() =
  // The map method of the List class is a typical example of a higher-order function—a function that takes a function as parameter.
  val a = List(1, 2, 3).map(i => i * 2)     // uses an explicit lambda expression as the mapping operation - i => i * 2 takes an integer i and multiplies it by 2.
  val b = List(1, 2, 3).map(_ * 2)          // uses placeholder syntax for the lambda expression - _ represents each element of the list.
  println(a)                                // List(2,4,6)
  println(b)                                // List(2,4,6)

  // The map methods could also take a method rather than a lambda function
  def double(i: Int): Int = i * 2
  val c = List(1, 2, 3).map(i => double(i)) // double(i) is called for each value of i, or alternatively
  val d = List(1, 2, 3).map(double)         // directly passing the double function as an argument to map.  
  println(c)                                // List(2,4,6)
  println(d)                                // List(2,4,6)

  // Immutable collections
  // When you work with immutable collections like List, Vector, and the immutable Map and Set classes
  // they return a new collection with the updated data. 
  // It’s also common to chain them together in a “fluent” style to solve problems.

  // Create a list
  val nums = (1 to 10).toList   // List(1,2,3,4,5,6,7,8,9,10)

  // methods can be chained together
  val x = nums.filter(_ > 3)
              .filter(_ < 7)
              .map(_ * 10)

  // result: x == List(40, 50, 60)
  println(x)