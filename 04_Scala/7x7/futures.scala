import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

// A method that simulates a long-running task
def longRunningTask(): Future[Int] = Future {
  // Simulate a task that takes time to complete
  Thread.sleep(1000)
  // Return some result
  42
}


@main def mainMethod(): Unit = {
    // Call the longRunningTask method and handle the result
    val futureResult = longRunningTask()

    futureResult.onComplete {
        case Success(value) => println(s"The result is $value")
        case Failure(exception) => println(s"An error has occurred: $exception")
    }

    // Continue with other operations without blocking
    println("This will print immediately, while the future is still running.")
}