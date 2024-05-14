object CompassApp {
  def main(args: Array[String]): Unit = {
    val myCompass = new Compass
    myCompass.turnRight()
    myCompass.turnRight()
    myCompass.turnLeft()
    myCompass.turnLeft()
    myCompass.turnLeft()
  }
}

class Compass {
    val directions = List("north", "east", "south", "west")
    var bearing = 0

    print("Initial bearing: ")
    println(direction())

    def direction(): String = directions(bearing)

    def inform(turnDirection: String): Unit = {
        println("Turning " + turnDirection + ". Now bearing " + direction())
    }

    def turnRight(): Unit = {
        bearing = (bearing + 1) % directions.size
        inform("right")
    }

    def turnLeft(): Unit = {
        bearing = (bearing + (directions.size - 1)) % directions.size
        inform("left")
    }
}


