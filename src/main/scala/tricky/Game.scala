package tricky

import Triangle._
import Types._

class Game(board: TriangleMatrix)(implicit dim: Int) extends TriangleLike {

  def getNextBoards: List[Game] = {
    getAvailableMoves.map(makeMove)
  }

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

  def isLegit(move: Move): Boolean = {
    val jumpedCoord = getJumped(move)

    move.start.isLegit &&
    jumpedCoord.isLegit &&
    move.end.isLegit &&
    board.get(move.start) == Peg && board.get(jumpedCoord) == Peg && board.get(move.end) == Empty
  }

  private def getJumped(move: Move): Coordinate = (move.start + move.end) / 2

  /** Finds a solution to the given Game
    *
    * @return A list of moves that describe how to get from the starting
    *         state to a state with only one peg on the board
    */
  def findSolution: List[Move] = ???
}