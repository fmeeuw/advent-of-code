package aoc2020

import util.InputOps

import scala.io.Source

object Day1 extends App {

  def part1 = {
    val numbers = parseInput()
    val pairs = for {
      x <- numbers
      y <- numbers.filter(y => y != x && y + x == 2020)
    } yield (x, y)

    val (a, b) = pairs.head
    println(a * b)
  }

  def part2 = {
    val numbers = parseInput()
    val pairs = for {
      x <- numbers
      y <- numbers.filter(y => y != x && y + x <= 2020)
      z <- numbers.filter(z => z != x && z != y && z + y + x == 2020)
    } yield (x, y, z)
    val (a, b, c) = pairs.head
    println(a * b * c)
  }

  def parseInput(): Vector[Int] = {
    InputOps.readLines(2020, 1).map(_.toInt).toVector
  }

  part1 // 494475
  part2 // 267520550
}