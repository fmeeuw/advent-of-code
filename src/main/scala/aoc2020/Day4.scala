package aoc2020

import util.InputOps

import scala.io.Source
import scala.util.Try

object Day4 extends App {

  def part1 = {
    val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    val matches = parseInput.count(passport => requiredKeys.subsetOf(passport.map(_._1).toSet))
    println(matches)
  }

  def part2 = {
    val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    def isValidPassport(passport: Vector[(String, String)]): Boolean = {
      requiredKeys.subsetOf(passport.map(_._1).toSet) &&
      passport.forall { case (key, value) => isValid(key, value) }
    }
    val matches = parseInput.count(isValidPassport)
    println(matches)
  }

  def parseInput: Vector[Vector[(String, String)]] = {
    val lines = InputOps.readLines(2020, 4).toVector

    val passportLines =
      lines.foldLeft(Vector("")) { (agg, line) =>
        if (line.isEmpty) "" +: agg
        else (agg.head + line + " ") +: agg.tail
      }

    passportLines.map { string =>
      val items = string.split(' ')
      items.map { item =>
        val Array(key, value) = item.split(":", 2)
        key -> value
      }.toVector
    }
  }

  /** byr (Birth Year) - four digits; at least 1920 and at most 2002. iyr (Issue Year) - four digits; at least 2010 and
    * at most 2020. eyr (Expiration Year) - four digits; at least 2020 and at most 2030. hgt (Height) - a number
    * followed by either cm or in: If cm, the number must be at least 150 and at most 193. If in, the number must be at
    * least 59 and at most 76. hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f. ecl (Eye Color) -
    * exactly one of: amb blu brn gry grn hzl oth. pid (Passport ID) - a nine-digit number, including leading zeroes.
    * cid (Country ID) - ignored, missing or not.
    */
  def isValid(key: String, value: String): Boolean = {
    def tryOrFalse(logic: => Boolean): Boolean = {
      Try(logic).toOption.exists(identity)
    }

    val result = key match {
      case "byr" =>
        tryOrFalse {
          value.size == 4 && value.toInt >= 1920 && value.toInt <= 2002
        }
      case "iyr" =>
        tryOrFalse {
          value.size == 4 && value.toInt >= 2010 && value.toInt <= 2020
        }
      case "eyr" =>
        tryOrFalse {
          value.size == 4 && value.toInt >= 2020 && value.toInt <= 2030
        }
      case "hgt" =>
        tryOrFalse {
          val height = value.dropRight(2).toInt
          if (value.endsWith("cm")) {
            height >= 150 && height <= 193
          } else if (value.endsWith("in")) {
            height >= 59 && height <= 76
          } else {
            false
          }
        }
      case "hcl" => value.matches("#[0-9a-f]{6}")
      case "ecl" => value.matches("amb|blu|brn|gry|grn|hzl|oth")
      case "pid" => value.matches("\\d{9}")
      case "cid" => true
    }
//    if (!result) { println(s"invalid value: $key $value") }
    result
  }

  part1 // 250
  part2 // 158
}
