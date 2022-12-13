package aoc2022

import aoc2022.Day8.readLines
import util.{AocApp, Grid, Point}

import scala.collection.mutable

object Day12 extends AocApp {

  def part1 = {
    val elevationLevels: Vector[Vector[Char]] = readLines().map(_.toVector).toVector
    val grid = Grid(elevationLevels)
    val startPosition = grid.points.find(point => grid.cell(point) == 'S').get
    val endPosition = grid.points.find(point => grid.cell(point) == 'E').get
    val steps = nextMoveRecursive(grid, 0, _ == endPosition, Map(0 -> List(startPosition)), Set.empty, uphill = true)

//    println(grid.toString)
    println(steps)
  }

  def part2 = {
    val elevationLevels: Vector[Vector[Char]] = readLines().map(_.toVector).toVector
    val grid = Grid(elevationLevels)
    val isStartPosition: Point => Boolean = point => grid.cell(point) == 'a' || grid.cell(point) == 'S'
    val endPosition = grid.points.find(point => grid.cell(point) == 'E').get

    // Move from end to start
    val steps = nextMoveRecursive(grid, 0, isStartPosition, Map(0 -> List(endPosition)), Set.empty, uphill = false)
    println(steps)
  }

  def nextMoveRecursive(
      grid: Grid[Char],
      currentSteps: Int,
      isDestination: Point => Boolean,
      todo: Map[Int, List[Point]],
      visited: Set[Point],
      uphill: Boolean
  ): Int = {
    val nextList = todo.get(currentSteps)
    nextList match
      case Some(::(head, next)) =>
        if (isDestination(head)) { currentSteps }
        else {
          val adjacents: List[Point] = grid
            .adjacent4Points(head)
            .filter(to => isReachable(grid, head, to, uphill))
            .filterNot(visited.contains)
//          println(
//            s"Currently evaluating point ${head}, visitable adjacents are: ${adjacents.mkString}, todo = ${todo.size} and visited = ${visited.size}"
//          )
          val updatedTodo = todo
            .updated(currentSteps, next)
            .updated(currentSteps + 1, (todo.getOrElse(currentSteps + 1, List.empty[Point]) ++ adjacents).distinct)
          nextMoveRecursive(grid, currentSteps, isDestination, updatedTodo, visited + head, uphill)
        }
      case Some(Nil) => nextMoveRecursive(grid, currentSteps + 1, isDestination, todo, visited, uphill)
      case None      => Int.MaxValue
  }

  def isReachable(grid: Grid[Char], from: Point, to: Point, uphill: Boolean) = {
    val sourceElevation = toElevationLevel(grid.cell(from))
    val targetElevation = toElevationLevel(grid.cell(to))
    if (uphill) targetElevation <= sourceElevation + 1 else targetElevation >= sourceElevation - 1
  }

  def toElevationLevel(char: Char): Int = {
    val elevationChar: Char = if (char == 'S') 'a' else if (char == 'E') 'z' else char
    elevationChar.toInt - 97
  }

  part1 // 437
  part2 // 430
}
