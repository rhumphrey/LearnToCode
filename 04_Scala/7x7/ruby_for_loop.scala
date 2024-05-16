object RubyForLoop {
  // This method prints each element of the 'args' array.
  def rubyStyleForLoop(args: Array[String]): Unit = {
    println("for loop using Ruby-style iteration")
    // 'foreach' is a higher-order function that takes a function as an argument.
    // Here, it takes a lambda function that takes 'arg' as a parameter and prints it.
    args.foreach { arg =>
      println(arg)
    }
  }

  // The 'main' method is the entry point of a Scala application.
  // This method is calling the 'rubyStyleForLoop' method with the 'args' array.
  def main(args: Array[String]): Unit = {
    rubyStyleForLoop(args)
  }
}
