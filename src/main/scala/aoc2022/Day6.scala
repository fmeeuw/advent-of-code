package aoc2022

import util.AocApp

object Day6 extends AocApp {

  def part1 = {
    val input: String = readLines().toList.head
//    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    val result =
      input.sliding(4).zipWithIndex.collectFirst { case (group, idx) if group.toSet.size == 4 => idx + 4 }
    println(result)
  }

  def part2 = {
    val input: String = readLines().toList.head
//    val input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    val result =
      input.sliding(14).zipWithIndex.collectFirst {
        case (group, idx) if group.toSet.size == 14 => idx + 14
      }
    println(result)
  }

  part1
  part2

}
