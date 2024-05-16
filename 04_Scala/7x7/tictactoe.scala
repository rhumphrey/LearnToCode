// Define a class for the TicTacToe game with a board represented as a 2D array of characters
class TicTacToe(board: Array[Array[Char]]) {

  // Method to check for a winner in the game
  def checkWinner(): Option[Char] = {
    // Convert each row of the board to a sequence and store them
    val rows = board.map(_.toSeq)
    // Convert each column of the board to a sequence and store them
    val cols = board.transpose.map(_.toSeq)
    // Create sequences for both diagonals of the board
    val diags = Seq(
      Seq(board(0)(0), board(1)(1), board(2)(2)), // Top-left to bottom-right diagonal
      Seq(board(0)(2), board(1)(1), board(2)(0))  // Top-right to bottom-left diagonal
    )

    // Combine all rows, columns, and diagonals and check if any line has the same character (indicating a win)
    // and is not just empty spaces. If a winner is found, return the winning character.
    (rows ++ cols ++ diags).find(line => line.distinct.length == 1 && line.head != ' ').map(_.head)
  }

  // Method to check if the game is a tie (no empty spaces left)
  def checkTie(): Boolean = {
    // Check if any row still contains an empty space, indicating the game is not a tie
    !board.exists(row => row.contains(' '))
  }

  // Method to get the current status of the game
  def getStatus: String = {
    // Use pattern matching to determine if there's a winner, a tie, or if the game is still ongoing
    checkWinner() match {
      case Some(winner) => s"Winner: $winner" // If a winner is found, return the winner
      case None if checkTie() => "Tie"        // If there's no winner but it's a tie, return "Tie"
      case _ => "No winner yet"                // If neither, the game is still ongoing
    }
  }
}

// Define an object with a main method to run the game
object Main extends App {
  // Initialize the board with a predefined configuration
  val board = Array(
    Array('O', 'X', 'X'),
    Array('O', 'O', 'O'),
    Array('X', 'X', 'O')
  )
  // Create a new game instance with the given board
  val game = new TicTacToe(board)
  // Print the current status of the game
  println(game.getStatus)
}


