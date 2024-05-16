// Define the main object extending the App trait to make it runnable
object TicTacToe extends App {
  // Initialize a 3x3 board with empty spaces
  val board = Array.fill(3,3)(' ')

  // Function to print the current state of the board
  def printBoard(): Unit = {
    for (row <- board) {
      // Print each row of the board separated by '|'
      println(row.mkString("|"))
      // Print a line to separate each row
      println("-----")
    }
  }

  // Function to check if a player has won
  def checkWin(player: Char): Boolean = {
    // Check for a winning row
    val rows = board.exists(row => row.forall(_ == player))
    // Check for a winning column by transposing the board and checking rows
    val cols = board.transpose.exists(col => col.forall(_ == player))
    // Check for a winning diagonal from top-left to bottom-right
    val diag1 = (0 until 3).forall(i => board(i)(i) == player)
    // Check for a winning diagonal from top-right to bottom-left
    val diag2 = (0 until 3).forall(i => board(i)(2 - i) == player)
    // Return true if any winning condition is met
    rows || cols || diag1 || diag2
  }

  // Function to check if the game is a draw
  def checkDraw(): Boolean = {
    // Check if all spaces on the board are filled
    board.forall(row => row.forall(_ != ' '))
  }

  // Function for a player to take a turn
  def takeTurn(player: Char): Unit = {
    // Prompt the player for their move
    print(s"Player $player's turn. Enter row and column numbers (0, 1, 2) separated by space: ")
    // Read the player's input and split it into row and column
    val input = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    // Check if the chosen position is empty
    if (board(input(0))(input(1)) == ' ') {
      // If empty, place the player's mark
      board(input(0))(input(1)) = player
    } else {
      // If not empty, prompt the player to try again
      println("That spot is taken. Try again.")
      takeTurn(player)
    }
  }

  // Start with player 'X'
  var currentPlayer = 'X'
  // Continue the game until there's a win or a draw
  while (!checkWin('X') && !checkWin('O') && !checkDraw()) {
    // Print the board before each turn
    printBoard()
    // Current player takes a turn
    takeTurn(currentPlayer)
    // Switch to the other player
    currentPlayer = if (currentPlayer == 'X') 'O' else 'X'
  }

  // Print the final state of the board
  printBoard()
  // Announce the result of the game
  if (checkWin('X')) println("Player X wins!")
  else if (checkWin('O')) println("Player O wins!")
  else println("It's a draw!")
}
