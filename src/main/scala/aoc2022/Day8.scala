package aoc2022

import util.{AocApp, Grid, Point}

object Day8 extends AocApp {

  def part1 = {
    val trees = readLines().map(_.map(char => Integer.parseInt(char.toString)).toVector).toVector
    val grid = Grid(trees)
    val visibleTrees = grid.points.filter { point =>
      treesUntilEdge(grid, point).exists(_.forall(otherTree => grid.cell(otherTree) < grid.cell(point)))
    }
    println(visibleTrees.size)
  }

  def part2 = {
    val trees = readLines().map(_.map(char => Integer.parseInt(char.toString)).toVector).toVector
    val grid = Grid(trees)
    val scenicScores = grid.points.map(point => scenicScore(grid, point))
    println(scenicScores.max)
  }

  def scenicScore(grid: Grid[Int], point: Point): Int = {
    val rows = treesUntilEdge(grid, point)
    rows.map(trees => viewingDistance(grid, point, trees)).product
  }

  def viewingDistance(grid: Grid[Int], point: Point, treesUntilEdge: List[Point]): Int = {
    treesUntilEdge match
      case ::(head, next) if (grid.cell(head)) < grid.cell(point)  => 1 + viewingDistance(grid, point, next)
      case ::(head, next) if (grid.cell(head)) == grid.cell(point) => 1
      case ::(head, next) if (grid.cell(head)) > grid.cell(point)  => 0
      case Nil                                                     => 0
  }

  def treesUntilEdge(grid: Grid[Int], point: Point): List[List[Point]] = {
    val rightRow = (for (x <- (point.x + 1 until grid.width)) yield Point(x, point.y)).toList
    val leftRow = (for (x <- (0 until point.x)) yield Point(x, point.y)).reverse.toList
    val bottomColumn = (for (y <- (point.y + 1 until grid.height)) yield Point(point.x, y)).toList
    val topColumn = (for (y <- (0 until point.y)) yield Point(point.x, y)).reverse.toList

    List(topColumn, rightRow, bottomColumn, leftRow)
  }

  part1 // 1688
  part2 // 410400
}
