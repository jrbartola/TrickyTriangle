package tricky

import Types._

object Triangle {

  /** A trait defining a triangle-like object
    *
    */
  trait TriangleLike {

    def getAvailableMoves: List[Move]

    def makeMove(move: Move): TriangleLike

    def isLegit(move: Move): Boolean
  }

  class TriangleMatrix(dim: Int, map: Map[Coordinate, Placement]) {

    /** Retrieves the piece located at row `r` and column `c`
      *
      * @param coord The coordinate to look for a piece in
      * @return A Peg object if there is a Peg at the coordinate,
      *         An Empty object if there is not Peg at the coordinate
      */
    def get(coord: Coordinate): Placement = {
      map.get(coord) match {
        case Some(_) => Peg
        case None => Empty
      }
    }

    /** Places a piece in the Matrix
      *
      * @param coord The coordinate to place the piece on
      * @param piece The piece to be placed
      *
      * @return A new Matrix object with `piece` added to the map
      */
    def place(coord: Coordinate, piece: Placement): TriangleMatrix = {
      require(coord.row + coord.col < dim)

      new TriangleMatrix(dim, map + (coord -> piece))
    }

    /** Retrieves all the empty coordinates in the matrix
      *
      * @return A list of all coordinates that are not in the matrix
      */
    def emptySquares: List[Coordinate] = {
      List.range(0, dim).flatMap { r =>
        List.range(0, dim - r).map { c =>
          Coordinate(r, c)
        }
      }.filter { case coord: Coordinate => get(coord) == Empty }
    }

    /** Gets the number of pegs left on the board
      *
      * @return
      */
    def pegs: Int = {
      List.range(0, dim).flatMap { r =>
        List.range(0, dim - r).map { c =>
          Coordinate(r, c)
        }
      }.count(coord => get(coord) == Peg)
    }

    override def toString(): String = {
      List.range(0, dim).map { r =>
        List.range(0, dim - r).map { c =>
          get(Coordinate(r, c)) match {
            case Peg => 'P'
            case Empty => '_'
          }
        }
      }.map(_.mkString(" ")).mkString("\n")
    }
  }
}