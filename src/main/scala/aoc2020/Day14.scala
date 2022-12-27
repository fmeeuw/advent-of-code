package aoc2020

import util.AocApp

import scala.collection.BitSet
import scala.util.matching.Regex

object Day14 extends AocApp {

  override val logOnDebug: Boolean = false

  case class ProgramState(mask: BitMask, assignments: Map[Long, Long])

  sealed trait Op
  case class Assignment(address: Long, value: Long) extends Op
  case class BitMask(mask: String) extends Op
  def parseInput: List[Op] = {
    val assignmentRegex = "mem\\[(\\d+)\\] = (\\d+)".r
    val maskRegex = "mask = ([0-9X]+)".r
    readLines().map { line =>
      if (assignmentRegex.matches(line)) {
        val hit = assignmentRegex.findFirstMatchIn(line).head
        Assignment(hit.group(1).toLong, hit.group(2).toLong)
      } else {
        val hit = maskRegex.findFirstMatchIn(line).head
        BitMask(hit.group(1))
      }
    }.toList
  }

  def part1: Unit = println {
    val endState = parseInput.foldLeft(ProgramState(mask = BitMask(""), assignments = Map.empty)) { (state, op) =>
      println(s"Performing op ${op} on state ${state}")
      op match
        case Assignment(address, value) =>
          state.copy(assignments = state.assignments.updated(address, applyBitMask(state.mask, value)))
        case mask: BitMask => state.copy(mask = mask)
    }

    endState.assignments.values.sum
  }

  def applyBitMask(mask: BitMask, value: Long): Long = {
    fromBinaryString(applyBitMask(mask, toBinaryString(value)))
  }

  def applyBitMask(mask: BitMask, value: String): String = {
    val result = mask.mask
      .zip(value)
      .map { case (masked, actual) =>
        masked match
          case 'X' => actual
          case '1' => '1'
          case '0' => '0'
      }
      .mkString
    println(result)
    result
  }

  def toBinaryString(value: Long): String = {
    val shortString = value.toBinaryString
    ("0" * (36 - shortString.size)) ++ shortString
  }

  def fromBinaryString(value: String): Long = {
    java.lang.Long.parseLong(value, 2)
  }

  println(toBinaryString(73))
  println(fromBinaryString("000000000000000000000000000001001001"))

  println(toBinaryString(2508973))
  println(fromBinaryString("000000000000001001100100100010101101"))

  println(fromBinaryString("000000011001000101000000000010001111"))

  part1

}
