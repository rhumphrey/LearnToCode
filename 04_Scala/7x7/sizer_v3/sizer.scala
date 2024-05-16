// The Await needs to be modified to the appropriate timeout as this will probably timeout
// Be careful which site you point this at - use one that allows scraping
import scala.concurrent._
import scala.io.Source
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import ExecutionContext.Implicits.global

object Sizer {
  // Regular expression pattern to match HTML anchor tags
  val linkPattern = "<a\\s+(?:[^>]*?\\s+)?href=([\"'])(.*?)\\1".r

  // Function to fetch links from a given URL
  def fetchLinks(url: String): Future[List[String]] = Future {
    Try {
      val content = Source.fromURL(url)
      try {
        val pageContent = content.mkString
        linkPattern.findAllIn(pageContent).matchData.map(_.group(2)).toList
      } finally content.close()
    }.getOrElse(List.empty[String])
  }

  // Recursive function to fetch sizes of pages and follow links
  def fetchSizeAndFollowLinks(url: String, visited: Set[String]): Future[(String, Int, List[String])] = {
    if (visited.contains(url)) {
      Future.successful((url, 0, List.empty))
    } else {
      fetchLinks(url).flatMap { links =>
        val futures = links.distinct.filterNot(visited.contains).map { link =>
          fetchSizeAndFollowLinks(link, visited + url)
        }
        Future.sequence(futures).map { results =>
          val pageSizes = results.map(_._2)
          val totalSize = pageSizes.sum
          (url, totalSize, links)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val rootUrl = "https://httpbin.org/"
    val visited = Set.empty[String]

    val result = fetchSizeAndFollowLinks(rootUrl, visited)

    result.onComplete {
      case Success((url, size, links)) =>
        println(s"Root URL: $url")
        println(s"Total size of all pages: $size bytes")
        println(s"Links found: ${links.mkString(", ")}")
      case Failure(exception) =>
        println(s"An error has occurred: ${exception.getMessage}")
    }

    Await.ready(result, 60.seconds)
  }
}
