// Import the necessary classes from the Akka actor package
import akka.actor.{Actor, ActorSystem, Props}

// Define case objects to represent messages that can be sent to Kid actors
case object Poke
case object Feed

// Define a Kid class that extends the Actor trait
class Kid extends Actor {
  // Override the receive method to define how the Kid actor responds to messages
  override def receive: Actor.Receive = {
    case Poke =>
      // When the Kid actor receives a Poke message, print these lines
      println("Ow...")
      println("Quit it...")
    case Feed =>
      // When the Kid actor receives a Feed message, print these lines
      println("Gurgle...")
      println("Burp...")
  }
}

// Define an object to serve as the entry point of the application
object MainApp {
  def main(args: Array[String]): Unit = {
    // Create an ActorSystem to manage the actors
    val system = ActorSystem("KidSystem")
    // Create two Kid actors within the system
    val bart = system.actorOf(Props[Kid], name = "Bart")
    val lisa = system.actorOf(Props[Kid], name = "Lisa")

    // Send a Poke message to Bart and a Feed message to Lisa
    bart ! Poke
    lisa ! Feed

    // Print a ready message to the console
    println("Ready to poke and feed...")
    // It's a good practice to gracefully shutdown the actor system
    system.terminate()
  }
}


