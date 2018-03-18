package tricky

object Types {
  sealed trait Placement
  case object Peg extends Placement
  case object Empty extends Placement

  case class Coordinate(row: Int, col: Int) {
    def isLegit(implicit dim: Int): Boolean = {
      row + col < dim && row >= 0 && col >= 0
    }
  }

  case class Move(start: Coordinate, end: Coordinate) {
    def getJumped: Coordinate = (start + end) / 2
  }

  /** Implicit class defining extended functionality for Coordinate objects
    *
    */
  implicit class RichCoordinate(coord: Coordinate) {
    def +(other: Coordinate): Coordinate = {
      Coordinate(coord.row + other.row, coord.col + other.col)
    }

    def -(other: Coordinate): Coordinate = {
      Coordinate(coord.row - other.row, coord.col - other.col)
    }

    def >(other: Coordinate): Boolean = {
      coord.row > other.row && coord.col > other.col
    }

    def /(other: Int): Coordinate = {
      Coordinate(coord.row / other, coord.col / other)
    }
  }
}