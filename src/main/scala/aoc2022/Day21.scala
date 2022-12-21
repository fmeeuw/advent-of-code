package aoc2022

import util.AocApp
object Day21 extends AocApp {

  /** root: pppw + sjmn dbpl: 5 cczh: sllz + lgvd zczc: 2 ptdq: humn - dvpt dvpt: 3 lfqf: 4 humn: 5 ljgn: 2 sjmn: drzm *
    * dbpl sllz: 4 pppw: cczh / lfqf lgvd: ljgn * ptdq drzm: hmdt - zczc hmdt: 32
    */

  case class Input(monkey: String, yell: Yell)

  sealed trait Yell
  case class Value(value: BigDecimal) extends Yell
  case class Plus(refA: String, refB: String) extends Yell
  case class Product(refA: String, refB: String) extends Yell
  case class Minus(refA: String, refB: String) extends Yell
  case class Divide(refA: String, refB: String) extends Yell

  def part1 = {
    val monkeyYells: List[Input] = parseInput()
    println(monkeyYells.mkString("\n"))

    val yellDefinitionsMap: Map[String, Yell] = monkeyYells.map(yell => yell.monkey -> yell.yell).toMap
    val (result, map) = substituteRec("root", yellDefinitionsMap, Map.empty)
    println(map)
    println(result)
  }

  def part2 = {
    val monkeyYells: List[Input] = parseInput()
    println(monkeyYells.mkString("\n"))

    val remainingMonkeyYells = monkeyYells.filter(input => input.monkey != "humn" && input.monkey != "root")
    val remainingYellDefinitionsMap: Map[String, Yell] =
      remainingMonkeyYells.map(yell => yell.monkey -> yell.yell).toMap
    val result1 = binarySearch(remainingYellDefinitionsMap, 0, Long.MaxValue)
    println(result1)
  }

  def binarySearch(monkeyYells: Map[String, Yell], lowest: Long, highest: Long): Long = {
    val center = lowest + (highest - lowest) / 2
    val (result1, result2) = myYellProduces(center, monkeyYells)
    println(
      s"In i=$center , result1=$result1, result2=$result2 , equal =${result1 == result2}, diff = ${result1 - result2}, lowerBound=${lowest}, higherBound=${highest}"
    )
    if (result1 == result2) {
      center
    } else {
      val diff = result1 - result2
      if (diff > 0) {
        binarySearch(monkeyYells, center, highest)
      } else {
        binarySearch(monkeyYells, lowest, center)
      }
    }
  }

  def myYellProduces(number: Long, monkeyYells: Map[String, Yell]): (BigDecimal, BigDecimal) = {
    val actualYells = monkeyYells.updated("humn", Value(number))
    val (result1, map1) = substituteRec("dbcq", actualYells, Map.empty)
    val (result2, _) = substituteRec("zmvq", actualYells, map1)
//    val (result1, map1) = substituteRec("pppw", actualYells, Map.empty)
//    val (result2, map2) = substituteRec("sjmn", actualYells, map1)
//    println(s"In i=$number , result1=$result1, result2=$result2 , equal =${result1 == result2}")
    (result1, result2)
  }

  def substituteRec(
      monkey: String,
      yellDefs: Map[String, Yell],
      yellValues: Map[String, BigDecimal]
  ): (BigDecimal, Map[String, BigDecimal]) = {

    def substituteTwo(a: String, b: String): (BigDecimal, BigDecimal, Map[String, BigDecimal]) = {
      val (valueA, updatedMapA) = substituteRec(a, yellDefs, yellValues)
      val (valueB, updatedMapB) = substituteRec(b, yellDefs, updatedMapA)
//      println(s"Substituted values for monkey: $a and monkey $b => resulting in values $valueA and $valueB")
      (valueA, valueB, updatedMapB)
    }

    yellValues.get(monkey) match
      case Some(value) => value -> yellValues
      case None =>
        val yell = yellDefs(monkey)
        yell match
          case Value(value) => substituteRec(monkey, yellDefs, yellValues.updated(monkey, value))
          case Plus(refA, refB) =>
            val (valueA, valueB, updatedMap) = substituteTwo(refA, refB)
            substituteRec(monkey, yellDefs, updatedMap.updated(monkey, valueA + valueB))
          case Product(refA, refB) =>
            val (valueA, valueB, updatedMap) = substituteTwo(refA, refB)
            substituteRec(monkey, yellDefs, updatedMap.updated(monkey, valueA * valueB))
          case Minus(refA, refB) =>
            val (valueA, valueB, updatedMap) = substituteTwo(refA, refB)
            substituteRec(monkey, yellDefs, updatedMap.updated(monkey, valueA - valueB))
          case Divide(refA, refB) =>
            val (valueA, valueB, updatedMap) = substituteTwo(refA, refB)
            substituteRec(monkey, yellDefs, updatedMap.updated(monkey, valueA / valueB))
  }

  def parseInput(suffix: Option[String] = None) = {
    readLines(suffix).toList.map { line =>
      val monkeyName = line.take(4)
      val remaining = line.drop(6)
      val yell = if (remaining.forall(_.isDigit)) {
        Value(remaining.toInt)
      } else if (remaining.contains("+")) {
        Plus(remaining.take(4), remaining.takeRight(4))
      } else if (remaining.contains("*")) {
        Product(remaining.take(4), remaining.takeRight(4))
      } else if (remaining.contains("/")) {
        Divide(remaining.take(4), remaining.takeRight(4))
      } else if (remaining.contains("-")) {
        Minus(remaining.take(4), remaining.takeRight(4))
      } else {
        throw new IllegalStateException("Invalid format")
      }
      Input(monkeyName, yell)
    }
  }

  part1 // 81075092088442
  part2 // 3349136384441
}
