object ForLoop {
  def forLoop(args: Array[String]): Unit = {
    println("for loop using Java-style iteration")
    for(i <- args.indices) {
      println(args(i))
    }
  }

  def main(args: Array[String]): Unit = {
    forLoop(args)
  }
}