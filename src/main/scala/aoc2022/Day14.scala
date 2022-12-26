package aoc2022

import util.{AocApp, Grid, Point}

import scala.annotation.tailrec
object Day14 extends AocApp {

  case class Line(from: Point, to: Point) {
    def allPoints: List[Point] = {
      if (to.x > from.x) {
        (from.x to to.x).map(x => Point(x, from.y)).toList
      } else if (to.x < from.x) {
        (to.x to from.x).map(x => Point(x, from.y)).toList
      } else if (to.y > from.y) {
        (from.y to to.y).map(y => Point(from.x, y)).toList
      } else if (to.y < from.y) {
        (to.y to from.y).map(y => Point(from.x, y)).toList
      } else ???
    }
  }
  def part1 = {
    val lines = parseLines
    println(lines)

    val points = lines.flatMap(line => List(line.from, line.to)).distinct
    println(points)
    val maxScanY = points.map(_.y).max
    val maxScanX = (points.map(_.x).max)
    println(s"maxX=$maxScanX, maxY=$maxScanY")

    val emptyCells = (0 to maxScanY).map { y =>
      (0 to maxScanX).map { x =>
        '.'
      }.toVector
    }.toVector
    val emptyGrid = Grid(emptyCells)

    val grid = lines
      .foldLeft(emptyGrid) { (agg, line) =>
        line.allPoints.foldLeft(agg) { (agg2, point) => agg2.updated(point, '#') }
      }
      .updated(Point(500, 0), '+')

    fallSandRec(grid, Point(500, 0), 1)
  }

  def part2 = {
    val lines = parseLines
    println(lines)

    val points = lines.flatMap(line => List(line.from, line.to)).distinct
    println(points)
    val maxScanY = points.map(_.y).max
    val maxScanX = (points.map(_.x).max)
    println(s"maxX=$maxScanX, maxY=$maxScanY")

    val emptyCells = (0 to maxScanY + 1).map { y =>
      (0 to maxScanX * 2).map { x =>
        '.'
      }.toVector
    }.toVector
    val emptyGrid = Grid(emptyCells)

    val grid = lines
      .foldLeft(emptyGrid) { (agg, line) =>
        line.allPoints.foldLeft(agg) { (agg2, point) => agg2.updated(point, '#') }
      }
      .updated(Point(500, 0), '+')

    fallSandRec(grid, Point(500, 0), 1)
  }

  def fallSandRec(grid: Grid[Char], sandStartingPoint: Point, units: Int): Grid[Char] = {
    val sandDestination = pourSandUntilRest(grid, sandStartingPoint)

    if (units % 100 == 0 || sandDestination == sandStartingPoint) {
      println(s"Unit $units **************")
      println(grid)
      println("***************************")
    }

    val updatedGrid = grid.updated(sandDestination, '+')
    if (sandDestination == sandStartingPoint) {
      updatedGrid
    } else {
      fallSandRec(updatedGrid, sandStartingPoint, units + 1)
    }
  }

  @tailrec
  def pourSandUntilRest(grid: Grid[Char], startPoint: Point): Point = {
    if (grid.cellOpt(startPoint.north()).contains('.')) {
      pourSandUntilRest(grid, startPoint.north())
    } else if (grid.cellOpt(startPoint.north().west()).contains('.')) {
      pourSandUntilRest(grid, startPoint.north().west())
    } else if (grid.cellOpt(startPoint.north().east()).contains('.')) {
      pourSandUntilRest(grid, startPoint.north().east())
    } else {
      startPoint
    }
  }

  def parseLines: List[Line] = {
    readLines().toList
      .map(line => line.split("->").toList.map(point => parsePoint(point.trim)))
      .flatMap(_.sliding(2).toList.map { case List(from, to) => Line(from, to) })
  }

  def parsePoint(string: String): Point = {
    string.split(",").toList match {
      case List(x, y) => Point(x.toInt, y.toInt)
    }
  }

//  part1
  part2
}
