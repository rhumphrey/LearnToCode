// Define a method to perform a chore and return the actions taken
def doChore(chore: String): String = chore match {
  case "clean dishes" => "scrub, dry"   // If the chore is cleaning dishes, return "scrub, dry"
  case "cook dinner" => "chop, sizzle"  // If the chore is cooking dinner, return "chop, sizzle"
  case _ => "whine, complain"           // For any other chore, return "whine, complain"
}

// Main method to execute the program
@main def mainMethod(): Unit = {
  println(doChore("clean dishes"))      // Calls doChore with "clean dishes" and prints the result
  println(doChore("mow lawn"))          // Calls doChore with "mow lawn" (not explicitly matched) and prints "whine, complain"
}
