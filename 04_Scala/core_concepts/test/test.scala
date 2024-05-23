// Define the main method at the top level
@main def run(): Unit =
  // You can assign the result to a variable
  val a = 2
  val b = 3
  val minValue = if a < b then a else b

  // You can also use them as method bodies 
  def compare(a: Int, b: Int): Int = 
    if a < b then -1 else if a == b then 0 else 1







