package aoc2020

import util.InputOps

import scala.io.Source

object Day6 extends App {

  def part1 = {
    val groups = parseInput
    val answers: Seq[Set[Char]] = groups.map { group =>
      val groupSet = group.map(person => person.toSet)
      groupSet.foldLeft(Set.empty[Char])(_.union(_))
    }

    val count = answers.map(_.size).sum
    println(count)
  }

  def part2 = {
    val groups = parseInput
    val answers: Seq[Set[Char]] = groups.map { group =>
      val groupSet = group.map(person => person.toSet)
      groupSet.foldLeft("abcdefghijklmnopqrstuvwxyz".toSet)(_.intersect(_))
    }

    val count = answers.map(_.size).sum
    println(count)
  }

  def parseInput: Vector[Vector[String]] = {
    InputOps
      .readLines(2020, 6)
      .foldLeft(Vector(Vector.empty[String])) { (agg, line) =>
        if (line.isEmpty) Vector.empty +: agg
        else (agg.head :+ line) +: agg.tail
      }
      .toVector
  }

  part1 // 6291
  part2 // 3052
}
