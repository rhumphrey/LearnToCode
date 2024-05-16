import scala.xml._

val moviesXML: Elem = 
  <movies>
    <movie>The Incredibles</movie>
    <movie>WALL E</movie>
    <short>Jack Jack Attack</short>
    <short>Geri's Game</short>
  </movies>

val movies: NodeSeq = moviesXML \ "_"

@main def mainMethod(): Unit = {
    movies.foreach { movie =>
        movie match {
            case elem: Elem if elem.label == "movie" => println(elem.text)
            case elem: Elem if elem.label == "short" => println(elem.text + " (short)")
            case _ => // Handle other cases or ignore
        }
    }
}