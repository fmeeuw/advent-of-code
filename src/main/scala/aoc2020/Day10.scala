package aoc2020

import util.InputOps

import scala.annotation.tailrec

object Day10 extends App {

  def part1 = {
    val input = parseInput.sorted
    val sortedAdapters = 0 +: input :+ (input.max + 3)
//    println(sortedAdapters)
    val diffMap = sortedAdapters
      .sliding(2)
      .map { case Vector(a, b) => b - a }
      .toVector
      .groupBy(identity)

    println(
      diffMap.getOrElse(1, Vector.empty).size * diffMap
        .getOrElse(3, Vector.empty)
        .size
    )
  }

  def part2 = {
    val inputAdapters = parseInput.sorted.toList
    val combos =
      combinations(Map(0 -> 1L).withDefaultValue(0), inputAdapters.max + 3)(
        (inputAdapters) :+ inputAdapters.max + 3
      )
    println(combos)
  }

  def parseInput: Vector[Int] =
    InputOps.readLines(2020, 10).map(_.toInt).toVector

  def combinations(cache: Map[Int, Long], lastToMatch: Int)(
      jolts: List[Int]
  ): Long =
    jolts match {
      case Nil => cache(lastToMatch)
      case jolt :: newJolts =>
        val arrangements = cache(jolt - 3) + cache(jolt - 2) + cache(jolt - 1)
        val newCache = cache + (jolt -> arrangements)
        combinations(newCache, lastToMatch)(newJolts)
    }

  part1 // 2574
  part2 // 2644613988352
}
