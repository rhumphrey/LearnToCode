import scala.concurrent._                   // Importing concurrency library
import scala.io.Source                      // Importing IO library for reading from URLs
import scala.concurrent.duration._          // Importing duration library for timeout
import scala.util.{Failure, Success}        // Importing utilities for handling success and failure
import ExecutionContext.Implicits.global    // Importing global execution context for futures

// Define an object named Sizer
object Sizer {
  // Main function which will be the entry point of the program
  def main(args: Array[String]): Unit = {
    // List of URLs to fetch content from
    val urls = List(
        "https://www.examples.com/",
        "https://www.scrapethissite.com/",
        "https://httpbin.org/"
    )

    // Create a list of futures that asynchronously fetch content from URLs
    val futures = for (url <- urls) yield Future {
        val content = Source.fromURL(url)       // Fetch content from URL
        try content.mkString.length             // Convert content to string and get length
        finally content.close()                 // Ensure the source is closed after operation
    }

    // Convert list of futures into a single future that contains a list of results
    val sizes = Future.sequence(futures)

    // When all futures complete, print their results or the exception if they fail
    sizes.onComplete {
        case Success(sizes) => sizes.foreach(println)                   // On success, print each size
        case Failure(exception) => 
            println(s"An error has occurred: ${exception.getMessage}")  // On failure, print error message
    }

    // Wait for all futures to complete or timeout after 10 seconds
    Await.ready(sizes, 10.seconds)
  }
}

