object RubyForLoop {
  def rubyStyleForLoop(args: Array[String]): Unit = {
  println("for loop using Ruby-style iteration")
  args.foreach { arg =>
    println(arg)
  }
}

  def main(args: Array[String]): Unit = {
    rubyStyleForLoop(args)
  }
}