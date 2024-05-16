import scala.concurrent._                   // Importing concurrency library
import scala.io.Source                      // Importing IO library for reading from URLs
import scala.concurrent.duration._          // Importing duration library for timeout
import scala.util.{Failure, Success}        // Importing utilities for handling success and failure
import ExecutionContext.Implicits.global    // Importing global execution context for futures

// Define an object named Sizer
object Sizer {
  // Regular expression pattern to match HTML anchor tags
  val linkPattern = "<a\\s+(?:[^>]*?\\s+)?href=([\"'])(.*?)\\1".r

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
        try {
          val pageContent = content.mkString   // Convert content to string
          val links = linkPattern.findAllIn(pageContent).matchData.toList // Find all matches of the link pattern
          (url, pageContent.length, links.size) // Return tuple with URL, page size, and number of links
        } finally content.close()              // Ensure the source is closed after operation
    }

    // Convert list of futures into a single future that contains a list of results
    val results = Future.sequence(futures)

    // When all futures complete, print their results or the exception if they fail
    results.onComplete {
        case Success(results) => 
            results.foreach { case (url, size, numberOfLinks) =>
                println(s"URL: $url, Page size: $size bytes, Number of links: $numberOfLinks")
            }
        case Failure(exception) => 
            println(s"An error has occurred: ${exception.getMessage}")  // On failure, print error message
    }

    // Wait for all futures to complete or timeout after 10 seconds
    Await.ready(results, 10.seconds)
  }
}
