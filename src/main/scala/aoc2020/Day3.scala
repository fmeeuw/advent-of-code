package aoc2020

import util.AocApp

import scala.io.Source

object Day3 extends AocApp {

  case class World(base: Vector[Vector[Boolean]]) {
    def isOpen(x: Int, y: Int) = {
      base(y)(x % base(y).length)
    }
    def height = base.size

  }

  def part1 = {
    val world: World = World(parseInput)
    val trees = countTreesRec(world, 0, 0, 3, 1)
    println(trees)
  }

  def part2 = {
    val world: World = World(parseInput)
    val result = 1L *
      countTreesRec(world, 0, 0, 1, 1) *
      countTreesRec(world, 0, 0, 3, 1) *
      countTreesRec(world, 0, 0, 5, 1) *
      countTreesRec(world, 0, 0, 7, 1) *
      countTreesRec(world, 0, 0, 1, 2)

    println(result)
  }

  def parseInput: Vector[Vector[Boolean]] = {
    readLines()
      .map(_.map(_ == '.').toVector)
      .toVector
  }

  def countTreesRec(world: World, x: Int, y: Int, deltaX: Int, deltaY: Int): Int = {
    if (y >= world.height) {
      0
    } else {
      countTreesRec(world, x + deltaX, y + deltaY, deltaX, deltaY) + (if (world.isOpen(x, y)) 0 else 1)
    }
  }

  part1 // 294
  part2 // 5774564250
}
