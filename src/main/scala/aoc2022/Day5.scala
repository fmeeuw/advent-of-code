package aoc2022

import util.AocApp

import java.util
import scala.collection.immutable.Queue

object Day5 extends AocApp {

  case class Input(stacks: Stacks, moves: List[Move])
  case class Stacks(stacks: Vector[List[Char]])
  case class Move(amount: Int, from: Int, to: Int)

  def part1: Unit = {
    val input = parseInput
    val endState = input.moves.foldLeft(input.stacks) { (stacks, move) =>
      val nextState = doMove(stacks, move)
//      println(s"Performing move ${move}")
//      println("*" * 30)
//      printStacks(nextState)
//      println("*" * 30)
      nextState
    }
    println(endState.stacks.map(stack => stack.head).mkString)
  }
  def part2: Unit = {
    val input = parseInput
    val endState = input.moves.foldLeft(input.stacks) { (stacks, move) =>
      val nextState = doMove2(stacks, move)
//      println(s"Performing move ${move}")
//      println("*" * 30)
//      printStacks(nextState)
//      println("*" * 30)
      nextState
    }
    println(endState.stacks.map(stack => stack.head).mkString)
  }

  def doMove(stacks: Stacks, move: Move) = {
    stacks.copy(stacks =
      stacks.stacks
        .updated(move.from - 1, stacks.stacks(move.from - 1).drop(move.amount))
        .updated(move.to - 1, stacks.stacks(move.from - 1).take(move.amount).reverse ++ stacks.stacks(move.to - 1))
    )
  }

  def doMove2(stacks: Stacks, move: Move) = {
    stacks.copy(stacks =
      stacks.stacks
        .updated(move.from - 1, stacks.stacks(move.from - 1).drop(move.amount))
        .updated(move.to - 1, stacks.stacks(move.from - 1).take(move.amount) ++ stacks.stacks(move.to - 1))
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

    def stackIndex(idx: Int) = (idx / 4)

    val max = lines.last.split("\\s+").filterNot(_.isBlank).toList.map(_.toInt).max
    val result = lines.reverse.drop(1).foldLeft(emptyStacks(max)) { (stacks, line) =>
      line.zipWithIndex
        .foldLeft(stacks) { case (stacks, (char, index)) =>
          if (char.isLetter) {
            val idx = stackIndex(index)
            stacks.updated(idx, char :: stacks(idx))
          } else stacks
        }
    }
    Stacks(result)
  }

  def emptyStacks(maxNumber: Int): Vector[List[Char]] = {
    (1 to maxNumber).map(_ => List.empty[Char]).toVector
  }
  def printStacks(stacks: Stacks): Unit = {
    val bottomLine = stacks.stacks.indices.map(key => s" $key ").mkString(" ")
    val maxItems = stacks.stacks.map(_.size).max
    val result: Seq[String] = for { i <- 0 until maxItems } yield {
      stacks.stacks.map(stack => stack.lift(i).map(char => s"[$char]").getOrElse("   ")).mkString(" ")
    }

    result.foreach(println)
    println(bottomLine)
  }

  part1 // SVFDLGLWV
  part2 // DCVTCVPCL
}
