// Define a method to calculate the factorial of a number
def factorial(n: Int): Int = n match {
  case 0 => 1                               // Base case: if n is 0, the factorial is 1
  case x if x > 0 => factorial(n - 1) * n   // Recursive case: multiply n by the factorial of n-1
}

// Main method to execute the program
@main def mainMethod(): Unit = {
  println(factorial(3))                     // Prints the factorial of 3
  println(factorial(0))                     // Prints the factorial of 0, which is the base case
}
