package aoc2022

import util.{AocApp, Direction4, Point}
import util.Direction4.*

import scala.annotation.tailrec

object Day9 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Move(direction: Direction4)

  case class Positions(head: Point, tail: Point)

  def part1 = {
    val moves: List[Move] = parseMoves
    debug(moves)

    val startPosition = Point(0, 0)

    val headNextPositions = moves
      .foldLeft(List(startPosition)) { (history, move) =>
        history.head.move4(move.direction) :: history
      }
      .reverse

    val tailPositions = simulate(startPosition, headNextPositions)

    debug(tailPositions)
    info(tailPositions.distinct.size)
  }

  def part2 = {
    val moves: List[Move] = parseMoves
    debug(moves)

    val startPosition = Point(0, 0)
    val headNextPositions = moves
      .foldLeft(List(startPosition)) { (history, move) =>
        history.head.move4(move.direction) :: history
      }
      .reverse

    val tail9Positions = simulate9(startPosition, headNextPositions, 1)

    debug(tail9Positions)
    info(tail9Positions.distinct.size)
  }

  private def parseMoves: List[Move] = readLines().flatMap { line =>
    val direction = line.charAt(0) match
      case 'U' => North
      case 'R' => East
      case 'D' => South
      case 'L' => West
    val steps = line.drop(2).toInt
    List.fill(steps)(Move(direction))
  }.toList

  @tailrec
  private def simulate9(start: Point, headNextPositions: List[Point], currentTail: Int): List[Point] = {
    if (currentTail == 10) {
      debug("Stopping currentTail=9")
      headNextPositions: List[Point]
    } else {
      val tailPositions = simulate(start, headNextPositions)
      debug(s"TailPositions for n=$currentTail: $tailPositions")
      simulate9(start, tailPositions, currentTail + 1)
    }
  }

  private def simulate(tailStart: Point, headNextPositions: List[Point]): List[Point] = {
    headNextPositions
      .foldLeft(List[Point](tailStart)) { case (history, head) =>
        val tail = history.head
        val newTail = if (isTwoStepsInDirection(head, tail, North)) {
          tail.north()
        } else if (isTwoStepsInDirection(head, tail, East)) {
          tail.east()
        } else if (isTwoStepsInDirection(head, tail, South)) {
          tail.south()
        } else if (isTwoStepsInDirection(head, tail, West)) {
          tail.west()
        } else if (!areTouching(head, tail) && head.x != tail.x && head.y != tail.y) {
          val firstStep = if (head.x > tail.x) tail.east() else tail.west()
          val secondStep = if (head.y > tail.y) firstStep.north() else firstStep.south()
          secondStep
        } else {
          tail // dont move
        }

        newTail :: history
      }
      .reverse
  }

  private def isTwoStepsInDirection(head: Point, tail: Point, direction: Direction4) = {
    tail.move4(direction, 2) == head
  }

  private def areTouching(head: Point, tail: Point) = Math.abs(head.x - tail.x) <= 1 && Math.abs(head.y - tail.y) <= 1

  part1 // 6044
  part2 // 2384

}
