package aoc2022

import util.{AocApp, Direction, Point}

import scala.annotation.tailrec

object Day9 extends AocApp {

  case class Move(direction: Direction)

  case class Positions(head: Point, tail: Point)

  def part1 = {
    val moves: List[Move] = parseMoves
    println(moves)

    val startPosition = Point(0, 0)

    val headNextPositions = moves
      .foldLeft(List(startPosition)) { (history, move) =>
        history.head.move(move.direction) :: history
      }
      .reverse

    val tailPositions = simulate(startPosition, headNextPositions)

    println(tailPositions)
    println(tailPositions.distinct.size)
  }

  def part2 = {
    val moves: List[Move] = parseMoves
    println(moves)

    val startPosition = Point(0, 0)
    val headNextPositions = moves
      .foldLeft(List(startPosition)) { (history, move) =>
        history.head.move(move.direction) :: history
      }
      .reverse

    val tail9Positions = simulate9(startPosition, headNextPositions, 1)

    println(tail9Positions)
    println(tail9Positions.distinct.size)
  }

  private def parseMoves: List[Move] = readLines().flatMap { line =>
    val direction = line.charAt(0) match
      case 'U' => Direction.Up
      case 'R' => Direction.Right
      case 'D' => Direction.Down
      case 'L' => Direction.Left
    val steps = line.drop(2).toInt
    List.fill(steps)(Move(direction))
  }.toList

  @tailrec
  private def simulate9(start: Point, headNextPositions: List[Point], currentTail: Int): List[Point] = {
    if (currentTail == 10) {
      println("Stopping currentTail=9")
      headNextPositions: List[Point]
    } else {
      val tailPositions = simulate(start, headNextPositions)
      println(s"TailPositions for n=$currentTail: $tailPositions")
      simulate9(start, tailPositions, currentTail + 1)
    }
  }

  private def simulate(tailStart: Point, headNextPositions: List[Point]): List[Point] = {
    headNextPositions
      .foldLeft(List[Point](tailStart)) { case (history, head) =>
        val tail = history.head
        val newTail = if (isTwoStepsInDirection(head, tail, Direction.Up)) {
          tail.up()
        } else if (isTwoStepsInDirection(head, tail, Direction.Right)) {
          tail.right()
        } else if (isTwoStepsInDirection(head, tail, Direction.Down)) {
          tail.down()
        } else if (isTwoStepsInDirection(head, tail, Direction.Left)) {
          tail.left()
        } else if (!areTouching(head, tail) && head.x != tail.x && head.y != tail.y) {
          val firstStep = if (head.x > tail.x) tail.right() else tail.left()
          val secondStep = if (head.y > tail.y) firstStep.up() else firstStep.down()
          secondStep
        } else {
          tail // dont move
        }

        newTail :: history
      }
      .reverse
  }

  private def isTwoStepsInDirection(head: Point, tail: Point, direction: Direction) = {
    tail.move(direction, 2) == head
  }

  private def areTouching(head: Point, tail: Point) = Math.abs(head.x - tail.x) <= 1 && Math.abs(head.y - tail.y) <= 1

  part1 // 6044
  part2 // 2384

}
