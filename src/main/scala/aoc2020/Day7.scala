package aoc2020

import util.InputOps

object Day7 extends App {

  case class Contains(color: String, amount: Int)
  case class Rule(color: String, containsBags: Vector[Contains])

  def part1 = {
    val rules = parseInput.map(parseRule).toVector
    println(bagsContaining(rules, "shiny gold").size)
  }

  def part2 = {
    val rules = parseInput.map(parseRule).toVector
    println(countChildBags(rules, "shiny gold"))
  }

  def parseInput: Iterator[String] =
    InputOps.readLines(2020, 7)

  // clear tan bags contain 5 bright purple bags, 1 pale black bag, 5 muted lime bags.
  // vibrant orange bags contain 5 muted plum bags.
  // dim tan bags contain no other bags.
  // clear silver bags contain 1 shiny gold bag, 5 bright beige bags.
  def parseRule(line: String): Rule = {
    val List(color, rest) = line.split(" bags contain").toList
    val contains = rest
      .split(",")
      .map { contains =>
        val words = contains.drop(1).split(" ").toList
        if (words == List("no", "other", "bags.")) None
        else {
          val number = Integer.parseInt(words.head)
          val color = words(1) + " " + words(2)
          Some(Contains(color, number))
        }
      }
      .toVector
      .flatten
    Rule(color, contains)
  }

  def bagsContaining(
      rules: Vector[Rule],
      color: String
  ): Set[String] = {
    val bagsThatDirectlyContain =
      rules.filter(_.containsBags.exists(_.color == color)).map(_.color).toSet
    bagsThatDirectlyContain ++ bagsThatDirectlyContain.flatMap(c => bagsContaining(rules, c))
  }

  def countChildBags(
      rules: Vector[Rule],
      color: String
  ): Long = {
    val rule = rules.find(_.color == color).get
    rule.containsBags.foldLeft(0L) { (agg, contains) =>
      agg + contains.amount + (contains.amount * countChildBags(
        rules,
        contains.color
      ))
    }

  }

  part1 // 259
  part2 // 45018

}
