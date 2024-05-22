import java.io._

@main def run(): Unit = {

    // Using PrintWriter
    val pw = new PrintWriter(new File("example.txt"))
    pw.write("Hello, Scala!")
    pw.close()

    // Using FileWriter and BufferedWriter for more control
    val file = new File("example.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("Hello, Scala !")
    bw.close()

}