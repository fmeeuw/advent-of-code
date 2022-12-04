package aoc2020

import util.InputOps

object Day24 extends App {

  sealed trait Direction
  object Direction {

    def parseMultiple(string: String): List[Direction] = {
      extract(string) match {
        case Some((direction, remaining)) => direction :: parseMultiple(remaining)
        case None                         => List.empty
      }
    }

    def extract(string: String): Option[(Direction, String)] = {
      if (string.startsWith("e")) {
        Some(Direction.East -> string.drop(1))
      } else if (string.startsWith("se")) {
        Some(Direction.SouthEast -> string.drop(2))
      } else if (string.startsWith("sw")) {
        Some(Direction.SouthWest -> string.drop(2))
      } else if (string.startsWith("w")) {
        Some(Direction.West -> string.drop(1))
      } else if (string.startsWith("nw")) {
        Some(Direction.NorthWest -> string.drop(2))
      } else if (string.startsWith("ne")) {
        Some(Direction.NorthEast -> string.drop(2))
      } else {
        None
      }
    }

    case object East extends Direction

    case object SouthEast extends Direction

    case object SouthWest extends Direction

    case object West extends Direction

    case object NorthWest extends Direction

    case object NorthEast extends Direction

    val allDirections = List(East, SouthEast, SouthWest, West, NorthWest, NorthEast)
  }

  case class HexPoint(x: Int, y: Int)
  object HexPoint {
    val Zero = HexPoint(0, 0)
  }

  def part1 = {
    val input = parseInput
//    input.foreach(println)
    val reachedPoints: Seq[HexPoint] = input.map(directions => travelDirections(HexPoint.Zero, directions))
//    println(reachedPoints)
    val byPoint: Map[HexPoint, Int] = reachedPoints.groupBy(identity).mapValues(_.size).toMap
//    println(byPoint)
    val blackCells = byPoint.foldLeft(0) { case (count, (point, occurences)) =>
      if (occurences % 2 != 0) count + 1 else count
    }

    println(blackCells)
  }

  def part2 = {
    val input = parseInput
    val reachedPoints: Seq[HexPoint] = input.map(directions => travelDirections(HexPoint.Zero, directions))
    val byPoint: Map[HexPoint, Int] = reachedPoints.groupBy(identity).mapValues(_.size).toMap
    val initialBlackCells = byPoint.collect {
      case (point, occurences) if (occurences % 2 != 0) => point
    }.toSet
    //    println(s"initial black cells = $initialBlackCells  size = ${initialBlackCells.size}")
    val finalBlackCells = flipCellsForDays(0, initialBlackCells, 100)
    println(finalBlackCells.size)
  }

  def parseInput: Seq[List[Direction]] = {
    InputOps.readLines(2020, 24).map(Direction.parseMultiple).toVector
  }

  def travelDirections(from: HexPoint, directions: List[Direction]): HexPoint = {
    directions match {
      case Nil     => from
      case x :: xs => travelDirections(travelDirection(from, x), xs)
    }
  }

  def travelDirection(from: HexPoint, direction: Direction): HexPoint = {
    direction match {
      case Direction.East      => from.copy(x = from.x + 1)
      case Direction.SouthEast => from.copy(y = from.y + 1)
      case Direction.SouthWest => from.copy(x = from.x - 1, y = from.y + 1)
      case Direction.West      => from.copy(x = from.x - 1)
      case Direction.NorthWest => from.copy(y = from.y - 1)
      case Direction.NorthEast => from.copy(x = from.x + 1, y = from.y - 1)
    }
  }

  def flipCellsForDays(day: Int, blackCells: Set[HexPoint], totalDays: Int): Set[HexPoint] = {
//    println(s"Day $day: ${blackCells.size}")
    if (day == totalDays) {
      blackCells
    } else {
      val updatedBlackCells = flipCells(blackCells)
      flipCellsForDays(day + 1, updatedBlackCells, totalDays)
    }
  }

  def neighbours(cell: HexPoint): List[HexPoint] = {
    Direction.allDirections.map(direction => travelDirection(cell, direction))
  }

  /** Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white. Any white tile
    * with exactly 2 black tiles immediately adjacent to it is flipped to black.
    */
  def flipCells(blackCells: Set[HexPoint]): Set[HexPoint] = {
    val blackCellsWithNeighbors = blackCells ++ blackCells.flatMap(neighbours)
    val flipped: Set[HexPoint] = blackCellsWithNeighbors.flatMap { pos =>
      val blackNeighbours = neighbours(pos).map(blackCells).count(isBlack => isBlack)
      if (blackCells(pos) && (blackNeighbours == 0 || blackNeighbours > 2)) {
        // flipped
        Some(pos)
      } else if (!blackCells(pos) && blackNeighbours == 2) {
        // flipped
        Some(pos)
      } else {
        // nothing
        None
      }
    }

    val updatedBlackCells = (blackCells -- flipped.filter(blackCells)) ++ flipped.filterNot(blackCells)
    updatedBlackCells
  }

  part1 // 391
  part2 // 3876
}
