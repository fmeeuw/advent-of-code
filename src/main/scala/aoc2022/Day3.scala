package aoc2022

import aoc2022.Day2.readLines
import util.AocApp

import scala.io.Source

object Day3 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Rucksack(part1: String, part2: String)

  def part1 = {
    val result = parseInput.map { sack =>
      val intersectionChars = sack.part1.toList.toSet.intersect(sack.part2.toList.toSet)
      priority(intersectionChars.toList.head)
    }.sum
    info(result)
  }

  def part2 = {
    val result = parseInput
      .map(sack => (sack.part1 ++ sack.part2).toSet)
      .grouped(3)
      .map { group =>
        val badge = group.reduceLeft((a, b) => a.intersect(b)).head
        priority(badge)
      }
      .sum
    info(result)
  }

  def priority(char: Char) = {
    if (char.isUpper) {
      char.toInt - 38
    } else {
      char.toInt - 96
    }
  }

  def parseInput: List[Rucksack] = {
    readLines().map { line =>
      Rucksack(line.take(line.length / 2), line.drop(line.length / 2))
    }.toList
  }

  part1 // 7848
  part2 // 2616
}
