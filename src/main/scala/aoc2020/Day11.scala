package aoc2020

import util.InputOps

object Day11 extends App {

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = {
      Point(
        x = x + other.x,
        y = y + other.y
      )
    }
  }
  case class Cell(seat: Boolean, occupied: Boolean) {
    def toChar: Char =
      if (seat) {
        if (occupied) '#' else 'L'
      } else '.'
  }
  case class Floor(
      cells: Vector[Vector[Cell]]
  ) {

    override def toString: String =
      cells.map(row => row.map(_.toChar).mkString).mkString("\n")

    def nrOccupied: Int = cells.map(row => row.count(_.occupied)).sum

    def cellOpt(point: Point): Option[Cell] =
      if (withinBounds(point)) Some(cell(point)) else None
    def cell(point: Point) = cells(point.y)(point.x)

    def withinBounds(point: Point): Boolean =
      point.y >= 0 && point.y < cells.size &&
        point.x >= 0 && point.x < cells(point.y).size

    def height: Int = cells.size

    def width: Int = cells(0).size

    def mapCells(f: (Point, Cell) => Cell): Floor = {
      copy(cells = cells.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (cell, x) =>
          f(Point(x, y), cell)
        }
      })
    }

    def diagonals: List[Point] = {
      List(
        Point(0, 1), // up
        Point(1, 1), // upright
        Point(1, 0), // right
        Point(1, -1), // downright
        Point(0, -1), // down
        Point(-1, -1), // downleft
        Point(-1, 0), // left
        Point(-1, 1) // upleft
      )
    }

    def adjacents(point: Point): List[Cell] = {
      diagonals
        .map(_ + point)
        .filter(point => withinBounds(point))
        .map(point => cell(point))
    }

    def firstSeatsInAllDirections(point: Point): List[Cell] = {
      def iterateUntilSeat(position: Point, delta: Point): Option[Cell] = {
        val newPoint: Point = position + delta
        cellOpt(newPoint).flatMap { cell =>
          if (cell.seat) Some(cell)
          else iterateUntilSeat(newPoint, delta)
        }
      }
      diagonals.flatMap(iterateUntilSeat(point, _))
    }

  }

  def part1: Unit = {
    val floor = Floor(parseInput)
    val endState = doRoundsUntilNoChanges(0, floor, performRound1)
    println(endState.nrOccupied)
  }

  def part2 = {
    val floor = Floor(parseInput)
    val endState = doRoundsUntilNoChanges(0, floor, performRound2)
    println(endState.nrOccupied)
  }

  def parseInput: Vector[Vector[Cell]] =
    InputOps
      .readLines(2020, 11)
      .map { line =>
        line.map {
          case '.' => Cell(false, false)
          case 'L' => Cell(true, false)
        }.toVector
      }
      .toVector

  def doRoundsUntilNoChanges(
      round: Int,
      currentState: Floor,
      performRound: Floor => Floor
  ): Floor = {
    println(
      s"In round ${round}, occupied: ${currentState.nrOccupied}, state: ${currentState}"
    )
    val nextState = performRound(currentState)
    if (nextState == currentState) {
      currentState
    } else {
      doRoundsUntilNoChanges(round + 1, nextState, performRound)
    }
  }

  def performRound1(state: Floor): Floor = {
    state.mapCells { (point, cell) =>
      val adjacents = state.adjacents(point)
      if (cell.seat && !adjacents.exists(_.occupied)) {
        cell.copy(occupied = true)
      } else if (cell.seat && cell.occupied && adjacents.count(_.occupied) >= 4) {
        cell.copy(occupied = false)
      } else {
        cell
      }
    }
  }

  def performRound2(state: Floor): Floor = {
    state.mapCells { (point, cell) =>
      val surroundingSeats: Seq[Cell] = state.firstSeatsInAllDirections(point)
      if (cell.seat && !surroundingSeats.exists(_.occupied)) {
        cell.copy(occupied = true)
      } else if (cell.seat && cell.occupied && surroundingSeats.count(_.occupied) >= 5) {
        cell.copy(occupied = false)
      } else {
        cell
      }
    }
  }

  part1 // 2476
  part2 // 2257

}
