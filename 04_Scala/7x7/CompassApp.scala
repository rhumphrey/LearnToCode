// Define an object named CompassApp which will contain the main method
object CompassApp {
  // Main method - entry point of the Scala application
  def main(args: Array[String]): Unit = {
    // Create a new instance of the Compass class
    val myCompass = new Compass
    // Call the turnRight method of the Compass instance twice
    myCompass.turnRight()
    myCompass.turnRight()
    // Call the turnLeft method of the Compass instance three times
    myCompass.turnLeft()
    myCompass.turnLeft()
    myCompass.turnLeft()
  }
}

// Define a class named Compass
class Compass {
    // A List holding the directions in order
    val directions = List("north", "east", "south", "west")
    // Variable to keep track of the current bearing index from the directions List
    var bearing = 0

    // Print the initial bearing when a Compass instance is created
    print("Initial bearing: ")
    println(direction())

    // Method to get the current direction based on the bearing
    def direction(): String = directions(bearing)

    // Method to inform the current turn direction and the new bearing
    def inform(turnDirection: String): Unit = {
        println("Turning " + turnDirection + ". Now bearing " + direction())
    }

    // Method to turn right: increment the bearing and adjust it based on the size of the directions List
    def turnRight(): Unit = {
        bearing = (bearing + 1) % directions.size
        inform("right")
    }

    // Method to turn left: decrement the bearing and adjust it based on the size of the directions List
    def turnLeft(): Unit = {
        bearing = (bearing + (directions.size - 1)) % directions.size
        inform("left")
    }
}



