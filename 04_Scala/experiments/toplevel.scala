// Based on examples from https://docs.scala-lang.org/scala3/book/taste-toplevel-definitions.html
// All kinds of definitions can be written at the “top level” of your source code files
import scala.collection.mutable.ArrayBuffer

enum Topping:
  case Cheese, Pepperoni, Mushrooms

import Topping.*
class Pizza:
  val toppings = ArrayBuffer[Topping]()

val p = Pizza()

extension (s: String)
  def capitalizeAllWords = s.split(" ").map(_.capitalize).mkString(" ")

val hwUpper = "hello, world".capitalizeAllWords

type Money = BigDecimal

// more definitions as desired ...

@main def myApp =
  p.toppings += Cheese
  println(p.toppings)
  println("show me the code".capitalizeAllWords)