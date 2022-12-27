package aoc2022

import aoc2022.Day2.readLines
import util.AocApp

object Day4 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Pair(a: Range, b: Range)

  def part1 = info(parseInput.count { case Pair(a, b) =>
    fullyContainedWithin(a, b)
  })

  def part2 = info(parseInput.count { case Pair(a, b) =>
    overlaps(a, b)
  })

  def parseInput: List[Pair] = {
    readLines()
      .map { line =>
        line.split(",").toList match
          case List(rangeA, rangeB) => Pair(parseRange(rangeA), parseRange(rangeB))
      }
  }.toList

  def parseRange(rangeString: String): Range = rangeString.split("-").toList match {
    case List(start, end) => Range.inclusive(start.toInt, end.toInt)
  }

  def fullyContainedWithin(a: Range, b: Range): Boolean = {
    a.start >= b.start && a.end <= b.end || b.start >= a.start && b.end <= a.end
  }

  def overlaps(a: Range, b: Range): Boolean = {
    !(a.end < b.start || a.start > b.end)
  }

  part1 // 542
  part2 // 900
}
