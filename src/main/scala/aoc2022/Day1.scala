package aoc2022

import util.InputOps

import scala.io.Source

object Day1 extends App {

  def part1 = parseInput.map(_.sum).max
  def part2 = parseInput.map(_.sum).sorted.takeRight(3).sum

  def parseInput: Seq[List[Int]] = {
    InputOps
      .readLines(2022, 1)
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
