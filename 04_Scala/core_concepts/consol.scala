import scala.io.StdIn.readLine

@main def run(): Unit = 
    print("Please enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

