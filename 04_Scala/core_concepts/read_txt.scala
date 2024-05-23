import scala.io.Source


@main def read(): Unit = {

// Open the file
val filename = "example.txt"
val bufferedSource = Source.fromFile(filename)

// Read the file line by line
for (line <- bufferedSource.getLines) {
  println(line)
}

// Don't forget to close the source
bufferedSource.close()

}