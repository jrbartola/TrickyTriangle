package tricky

import Triangle._
import Types._

class Game(board: TriangleMatrix)(implicit dim: Int) extends TriangleLike {

  /** Retrieves a list of all possible games that could result
    * from making a move in the current board state
    *
    * @return A list of Games
    */
  def getNextBoards: List[Game] = {
    getAvailableMoves.map(makeMove)
  }

  /** Retrieves a list of all valid moves that could be made
    * from the current board state
    *
    * @return A list of Moves
    */
  def getAvailableMoves: List[Move] = {
    board.emptySquares.flatMap { coord =>
      List(Move(coord, coord - Coordinate(2, 0)),
        Move(coord, coord - Coordinate(2, 2)),
        Move(coord, coord - Coordinate(0, 2)),
        Move(coord, coord - Coordinate(-2, 2)),
        Move(coord, coord - Coordinate(2, -2)),
        Move(coord, coord + Coordinate(2, 0)),
        Move(coord, coord + Coordinate(2, 2)),
        Move(coord, coord + Coordinate(0, 2)))

    }.filter { case m: Move => isLegit(m) }
  }

  /** Makes a move on the current board state
    *
    * @param move The move to be made
    *
    * @return A new Game with a state reflecting the move that was just made.
    *         If the move is invalid, the current Game is returned
    */
  def makeMove(move: Move): Game = {
    if (!isLegit(move)) {
      this
    } else {
      val jumpedCoord = getJumped(move)

      val newMatrix = board.place(move.start, Empty)
                           .place(jumpedCoord, Empty)
                           .place(move.end, Peg)

      new Game(newMatrix)
    }
  }

  /** Determines if a Move is legitimate or not
    *
    * @param move The move to test
    *
    * @return true if the move is legitimate,
    *         false otherwise
    */
  def isLegit(move: Move): Boolean = {
    val jumpedCoord = getJumped(move)

    move.start.isLegit &&
    jumpedCoord.isLegit &&
    move.end.isLegit &&
    board.get(move.start) == Peg && board.get(jumpedCoord) == Peg && board.get(move.end) == Empty
  }

  /** Finds a solution to the given Game
    *
    * @return A list of moves that describe how to get from the starting
    *         state to a state with only one peg on the board
    */
  def findSolution: List[Move] = ???
}