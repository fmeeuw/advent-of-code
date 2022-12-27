package aoc2022

import util.AocApp

object Day11 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Monkey(items: Vector[Long], operation: Long => Long, test: BigInt => Int, totalInspections: Long)

  val testMonkeys: List[Monkey] = List(
    Monkey(Vector(79L, 98L), level => level * 19, level => if (level % 23 == 0) 2 else 3, 0),
    Monkey(Vector(54L, 65L, 75L, 74L), level => level + 6, level => if (level % 19 == 0) 2 else 0, 0),
    Monkey(Vector(79L, 60L, 97L), level => level * level, level => if (level % 13 == 0) 1 else 3, 0),
    Monkey(Vector(74L), level => level + 3, level => if (level % 17 == 0) 0 else 1, 0)
  )
  val testLeastCommonMultiple: Long = 23L * 19L * 13L * 17L

  val actualMonkeys = List(
    Monkey(Vector(65, 58, 93, 57, 66), level => level * 7, level => if (level % 19 == 0) 6 else 4, 0),
    Monkey(Vector(76, 97, 58, 72, 57, 92, 82), level => level + 4, level => if (level % 3 == 0) 7 else 5, 0),
    Monkey(Vector(90, 89, 96), level => level * 5, level => if (level % 13 == 0) 5 else 1, 0),
    Monkey(Vector(72, 63, 72, 99), level => level * level, level => if (level % 17 == 0) 0 else 4, 0),
    Monkey(Vector(65), level => level + 1, level => if (level % 2 == 0) 6 else 2, 0),
    Monkey(Vector(97, 71), level => level + 8, level => if (level % 11 == 0) 7 else 3, 0),
    Monkey(Vector(83, 68, 88, 55, 87, 67), level => level + 2, level => if (level % 5 == 0) 2 else 1, 0),
    Monkey(Vector(64, 81, 50, 96, 82, 53, 62, 92), level => level + 5, level => if (level % 7 == 0) 3 else 0, 0)
  )
  val actualLeastCommonMultiple: Long = 19L * 3L * 13L * 17L * 2L * 11L * 5L * 7L

  def part1 = {
    val endState = play(actualMonkeys, 20, level => level / 3)
    val monkeyBusiness = calculateMonkeyBusiness(endState)
    info(monkeyBusiness)
  }

  def part2 = {
    val endState = play(actualMonkeys, 10000, level => level % actualLeastCommonMultiple)
    val monkeyBusiness = calculateMonkeyBusiness(endState)
    info(monkeyBusiness)
  }

  def calculateMonkeyBusiness(monkeys: List[Monkey]): Long = monkeys.map(_.totalInspections).sorted.takeRight(2).product

  def play(monkeys: List[Monkey], rounds: Int, adjustWorryLevel: Long => Long) = {
    debug("Starting with")
    printMonkeys(monkeys)
    (1 to rounds).foldLeft(monkeys) { (agg, roundNr) =>
      val result = doRound(agg, adjustWorryLevel)
      if (roundNr % 1000 == 0 || roundNr == 20 || roundNr == 1) {
        debug(s"After round $roundNr the following items are holded per monkey:")
        printMonkeys(result)
      }
      result
    }
  }
  def doRound(monkeys: List[Monkey], adjustWorryLevel: Long => Long) = {
    monkeys.indices.foldLeft(monkeys) { (monkeys, monkeyIdx) =>
      handleItemsRec(monkeys, monkeyIdx, adjustWorryLevel)
    }
  }

  def handleItemsRec(monkeys: List[Monkey], currentMonkeyIdx: Int, adjustWorryLevel: Long => Long): List[Monkey] = {
    val monkey = monkeys(currentMonkeyIdx)
    monkey.items.headOption match
      case None => monkeys
      case Some(item) => {
        debug(s"\ttMonkey $currentMonkeyIdx inspects an item with a worry level of $item.")
        val updatedWorryLevel = adjustWorryLevel(monkey.operation(item))
        debug(s"\t\tUpdated worry level: $updatedWorryLevel.")
        val throwToMonkey = monkey.test(updatedWorryLevel)
        debug(s"\t\tItem with worry level ${updatedWorryLevel} is thrown to monkey ${throwToMonkey}.")

        val updatedMonkeys = monkeys
          .updated(
            currentMonkeyIdx,
            monkey.copy(
              items = monkey.items.tail,
              totalInspections = monkey.totalInspections + 1
            )
          )
          .updated(
            throwToMonkey,
            monkeys(throwToMonkey).copy(
              items = monkeys(throwToMonkey).items :+ updatedWorryLevel
            )
          )
        handleItemsRec(updatedMonkeys, currentMonkeyIdx, adjustWorryLevel)
      }
  }

  def printMonkeys(monkeys: List[Monkey]): Unit = {
    debug(s"**************** MONKEYS *********************")
    monkeys.zipWithIndex.foreach { case (monkey, idx) =>
      debug(s"Monkey $idx: ${monkey.items.mkString(",")}, totalInspections: ${monkey.totalInspections}")
    }
    debug(s"***********************************************")

  }

  part1 // 61503
  part2 // 14081365540
}
