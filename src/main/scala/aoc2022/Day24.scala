package aoc2022

import aoc2022.Day23.readLines
import util.{AocApp, Point}
import util.Direction4

object Day24 extends AocApp {

  case class Blizzards(north: Set[Point], east: Set[Point], south: Set[Point], west: Set[Point]) {
    def contains(point: Point) =
      north.contains(point) || east.contains(point) || south.contains(point) || west.contains(point)
  }

  case class State(start: Point, end: Point, blizzards: Blizzards, width: Int, height: Int) {
    def isBlizzard(point: Point) = blizzards.contains(point)

    def isWall(point: Point): Boolean =
      if (point == start || point == end) {
        false
      } else if (point.x <= 0 || point.x >= width - 1 || point.y <= 0 || point.y >= height - 1) {
        true
      } else {
        false
      }

    def isFree(point: Point): Boolean = !isBlizzard(point) && !isWall(point)

    def next: State = {
      val nextNorths = blizzards.north.map { point =>
        val next = point.north()
        if (isWall(next)) Point(next.x, 1) else next
      }
      val nextEasts = blizzards.east.map { point =>
        val next = point.east()
        if (isWall(next)) Point(1, next.y) else next
      }
      val nextSouths = blizzards.south.map { point =>
        val next = point.south()
        if (isWall(next)) Point(next.x, height - 2) else next
      }
      val nextWests = blizzards.west.map { point =>
        val next = point.west()
        if (isWall(next)) Point(width - 2, next.y) else next
      }
      copy(blizzards = Blizzards(nextNorths, nextEasts, nextSouths, nextWests))
    }
  }
  def part1 = {
    val state = parseInput()
//    println("Initial state")
//    printState(state)
    val (_, minutes) = move(state.end, List(MoveState(state.start, state, 0)))
    println(minutes)
  }

  def part2 = {
    val state = parseInput()
//    println("Initial state")
//    printState(state)
    val (endState1, minutes1) = move(state.end, List(MoveState(state.start, state, 0)))
    println(s"First reach end in $minutes1")
    val (endState2, minutes2) = move(state.start, List(MoveState(state.end, endState1, minutes1)))
    println(s"Secondly reach start in $minutes2")
    val (_, minutes3) = move(state.end, List(MoveState(state.start, endState2, minutes2)))
    println(s"Thirdly reach end again in $minutes3")
  }

  case class MoveState(pos: Point, state: State, minutes: Int)
  def move(target: Point, todo: List[MoveState]): (State, Int) = {
    todo match
      case ::(MoveState(pos, state, minutes), next) =>
//        println(s"In move @ minute $minutes and pos $pos.")
//        printState(state)
//        println(s"*" * 30)
        if (pos == target) {
          state -> minutes
        } else {
          val nextMoves =
            (pos.adjacents4() :+ pos).filter(state.next.isFree).map(pos => MoveState(pos, state.next, minutes + 1))
//          println(s"Possible moves = ${(pos.adjacents4() :+ pos).filter(state.next.isFree)}.")
//          println(s"Next moves = ${(nextMoves
//              .filterNot(next.contains) ++ next).sortBy(_.minutes).map(state => state.pos -> state.minutes)} ")
          move(target, (nextMoves.filterNot(next.contains) ++ next).sortBy(_.minutes))
        }
      case Nil => throw new IllegalStateException("Arg")
  }

  def printState(state: State): Unit = {
    val string = Range
      .inclusive(state.height - 1, 0, -1)
      .toVector
      .map(y =>
        y + "\t" + (0 until state.width).toVector
          .map(x => if (state.isWall(Point(x, y))) '#' else if (state.isBlizzard(Point(x, y))) '*' else '.')
          .map(_.toString)
          .mkString
      )
      .mkString("\n")

    println(string)
  }

  def parseInput(suffix: Option[String] = None): State = {
    val linesWithIndex = readLines(suffix).toList.reverse.zipWithIndex

    println(linesWithIndex)

    val blizzardsList: List[(Direction4, Point)] = linesWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (char, x) =>
        if (char == '>') Some(Direction4.East -> Point(x, y))
        else if (char == '<') Some(Direction4.West -> Point(x, y))
        else if (char == '^') Some(Direction4.North -> Point(x, y))
        else if (char == 'v') Some(Direction4.South -> Point(x, y))
        else None
      }
    }.toList

    val end = linesWithIndex.head match
      case (line, y) => Point(line.zipWithIndex.find(_._1 == '.').get._2, y)

    val start = linesWithIndex.last match
      case (line, y) => Point(line.zipWithIndex.find(_._1 == '.').get._2, y)

    println(start)
    println(end)

    val blizzards = Blizzards(
      north = blizzardsList.collect { case (dir, point) if dir == Direction4.North => point }.toSet,
      east = blizzardsList.collect { case (dir, point) if dir == Direction4.East => point }.toSet,
      south = blizzardsList.collect { case (dir, point) if dir == Direction4.South => point }.toSet,
      west = blizzardsList.collect { case (dir, point) if dir == Direction4.West => point }.toSet
    )

    State(start, end, blizzards, linesWithIndex.head._1.size, linesWithIndex.size)
  }

  part1 // 297
  part2 // 856
}
