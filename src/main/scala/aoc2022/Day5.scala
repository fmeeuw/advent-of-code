package aoc2022

import util.AocApp

import java.util
import scala.collection.immutable.Queue

object Day5 extends AocApp {

  case class Input(stacks: Stacks, moves: List[Move])
  case class Stacks(stacks: Map[Int, Queue[Char]]) // TODO move out of case class and to Array[List or something
  case class Move(amount: Int, from: Int, to: Int)

  def part1: Unit = {
    val input = parseInput
    val endState = input.moves.foldLeft(input.stacks) { (stacks, move) =>
      val nextState = doMove(stacks, move)
      println(s"Performing move ${move}")
      println("*" * 30)
      printStacks(nextState)
      println("*" * 30)
      nextState
    }
    println(endState.stacks.keys.toList.sorted.map(s => endState.stacks(s).head).mkString)
  }
  def part2: Unit = {
    val input = parseInput
    val endState = input.moves.foldLeft(input.stacks) { (stacks, move) =>
      val nextState = doMove2(stacks, move)
      println(s"Performing move ${move}")
      println("*" * 30)
      printStacks(nextState)
      println("*" * 30)
      nextState
    }
    println(endState.stacks.keys.toList.sorted.map(s => endState.stacks(s).head).mkString)
  }

  def doMove(stacks: Stacks, move: Move) = {
    stacks.copy(stacks =
      stacks.stacks
        .updated(move.from, stacks.stacks(move.from).drop(move.amount))
        .updated(move.to, stacks.stacks(move.from).take(move.amount).reverse ++ stacks.stacks(move.to))
    )
  }

  def doMove2(stacks: Stacks, move: Move) = {
    stacks.copy(stacks =
      stacks.stacks
        .updated(move.from, stacks.stacks(move.from).drop(move.amount))
        .updated(move.to, stacks.stacks(move.from).take(move.amount) ++ stacks.stacks(move.to))
    )
  }

  def parseInput: Input = {
    val lines = readLines().toList
    val moveRegex = "move (\\d+) from (\\d) to (\\d)".r
    val initialStacks = parseStacks(lines.takeWhile(line => !moveRegex.matches(line)).dropRight(1))
    val moves = lines
      .dropWhile(line => !moveRegex.matches(line))
      .map { line =>
        val hit = moveRegex.findFirstMatchIn(line).get
        Move(hit.group(1).toInt, hit.group(2).toInt, hit.group(3).toInt)
      }
    Input(stacks = initialStacks, moves = moves)
  }

  def parseStacks(lines: List[String]): Stacks = {

    def stackIndex(idx: Int) = (idx / 4) + 1

    val result = lines.reverse.drop(1).foldLeft(Map.empty[Int, Queue[Char]]) { (stacks, line) =>
      line.zipWithIndex
        .foldLeft(stacks) { case (stacks, (char, index)) =>
          if (char.isLetter) {
            stacks.updatedWith(stackIndex(index)) {
              case None           => Some(Queue.apply[Char](char))
              case Some(existing) => Some(existing.prepended(char))
            }
          } else stacks
        }
    }
    Stacks(result)
  }
  def printStacks(stacks: Stacks): Unit = {
    val sortedKeys = stacks.stacks.keys.toList.sorted
    val bottomLine = sortedKeys.map(key => s" $key ").mkString(" ")
    val maxItems = stacks.stacks.values.map(_.size).max
    val result: Seq[String] = for { i <- 0 until maxItems } yield {
      sortedKeys.map(s => stacks.stacks(s).lift(i).map(char => s"[$char]").getOrElse("   ")).mkString(" ")
    }

    result.foreach(println)
    println(bottomLine)
  }

  part1
  part2
}
