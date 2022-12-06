package aoc2022

import util.AocApp

object Day6 extends AocApp {

  def part1 = {
    val input: String = readLines().toList.head
//    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    println(indexOffirstUniqueSequence(input, 4).get)
  }

  def part2 = {
    val input: String = readLines().toList.head
//    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    println(indexOffirstUniqueSequence(input, 14).get)
  }

  private def indexOffirstUniqueSequence(input: String, n: Int) = input.sliding(n).zipWithIndex.collectFirst {
    case (group, idx) if group.toSet.size == n => idx + n
  }

  part1 // 1802
  part2 // 3551

}
