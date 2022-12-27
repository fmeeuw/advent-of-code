package aoc2020

import util.AocApp

object Day9 extends AocApp {

  override val logOnDebug: Boolean = false

  def part1 = {
    val input = parseInput
    val result = input
      .sliding(25 + 1)
      .find { seq =>
        val (preamble, number) = seq.splitAt(25)
        !isValidNumber(preamble, number.head)
      }
      .get
      .last
    println(result)
  }

  def part2 = {
    val input = parseInput
    val numberToFind: Long = 466456641L

    val solutions = for {
      i <- 0 to input.size
      size <- 2 until 20
      sum = input.slice(i, i + size).sum
      if sum == numberToFind
    } yield input.slice(i, i + size)

    val solution = solutions.head
//    println(solution)
    println(solution.min + solution.max)
  }

  def parseInput: Vector[Long] =
    readLines().map(_.toLong).toVector

  def isValidNumber(preamble: Vector[Long], number: Long): Boolean = {
    preamble.combinations(2).exists(_.sum == number)
  }

  part1 // 466456641
  part2 // 55732936
}
