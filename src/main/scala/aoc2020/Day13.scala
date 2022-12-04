package aoc2020

import util.InputOps

import scala.util.{Success, Try}

object Day13 extends App {

  case class Input(departureTimestamp: Long, busIds: List[Int])

  def part1 = {
    val input = parseInput
    val (timestamp, busId) =
      findMatchingBusRec(input.departureTimestamp, input.busIds)
    val waitingTime = timestamp - input.departureTimestamp
//    println(
//      s"depart at $timestamp with bus $busId, time to wait = ${waitingTime}, result = ${waitingTime * busId}"
//    )
    println(waitingTime * busId)
  }

  def part2 = {
    val busIds = parseInput2.toList
    println(
      chineseRemainder(
        busIds.map(_._1),
        busIds.map { case (id, idx) => id - idx }
      ).get
    )
  }

  def parseInput: Input = {
    val List(timestampString, busIdString) =
      InputOps.readLines(2020, 13).toList
    val busIds = busIdString
      .split(",")
      .toList
      .flatMap(idString => idString.trim.toIntOption)
    Input(timestampString.toLong, busIds)
  }

  def findMatchingBusRec(timestamp: Long, busIds: List[Int]): (Long, Int) = {
    val matchingBusOpt = busIds.find { timestamp % _ == 0 }
    matchingBusOpt match {
      case None              => findMatchingBusRec(timestamp + 1, busIds)
      case Some(matchingBus) => timestamp -> matchingBus
    }
  }

  def parseInput2: Map[Long, Int] = {
    val List(_, busIdString) = InputOps.readLines(2020, 13).toList
    busIdString
      .split(",")
      .toList
      .map(idString => idString.trim.toLongOption)
      .zipWithIndex
      .collect { case (Some(busId), idx) => busId -> idx }
      .toMap
  }

  // Taken from https://rosettacode.org/wiki/Chinese_remainder_theorem adjusted to Longs
  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  part1 // 119
  part2 // 1106724616194525
}
