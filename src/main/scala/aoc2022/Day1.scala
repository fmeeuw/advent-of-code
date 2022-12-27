package aoc2022

import util.AocApp

import scala.io.Source

object Day1 extends AocApp {

  override val logOnDebug: Boolean = false

  def part1 = {
    val result = parseInput.map(_.sum).max
    info(result)
  }
  def part2 = {
    val result = parseInput.map(_.sum).sorted.takeRight(3).sum
    info(result)
  }

  def parseInput: Seq[List[Int]] = {
    readLines()
      .foldLeft(List(List.empty[Int])) { (agg, elem) =>
        if (elem.isBlank) {
          List.empty :: agg
        } else {
          (agg.head :+ elem.toInt) :: agg.tail
        }
      }
      .reverse
  }

  println(part1) // 72718
  println(part2) // 213089
}
