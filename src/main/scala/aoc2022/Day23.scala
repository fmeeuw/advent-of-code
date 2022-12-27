package aoc2022

import util.Direction8.*
import util.{AocApp, Direction8, Point}
import util.Grid
import scala.annotation.tailrec
import scala.collection.immutable

object Day23 extends AocApp {

  override val logOnDebug: Boolean = false

  def part1 = {
    val elves = parseInput()
    debug(elves)
    val (_, movedElves) = doRoundRec(10, 1, elves, List(North, South, West, East))

    printElves(movedElves)
    val result = calcScore(movedElves)
    info(result)
  }

  def part2 = {
    val elves = parseInput()
    debug(elves)
    val (round, _) = doRoundRec(java.lang.Long.MAX_VALUE, 1, elves, List(North, South, West, East))
    info(round)
  }

  def calcScore(elves: List[Point]): Int = {
    val maxX = elves.map(_.x).max
    val minX = elves.map(_.x).min
    val maxY = elves.map(_.y).max
    val minY = elves.map(_.y).min

    val rectangleSquares = (maxY - minY + 1) * (maxX - minX + 1)
    debug(s"Rectangle = ($maxY - $minY + 1) * ($maxX - $minX + 1) = $rectangleSquares")
    rectangleSquares - elves.size
  }

  def printElves(elves: List[Point]): Unit = {
    val maxX = elves.map(_.x).max
    val minX = elves.map(_.x).min
    val maxY = elves.map(_.y).max
    val minY = elves.map(_.y).min

    val string = Range
      .inclusive(maxY, minY, -1)
      .toVector
      .map(y =>
        y + "\t" + (minX to maxX).toVector
          .map(x => if (elves.contains(Point(x, y))) '#' else '.')
          .map(_.toString)
          .mkString
      )
      .mkString("\n")

    debug(string)
  }

  @tailrec
  def doRoundRec(
      totalRounds: Long,
      currentRound: Long,
      elves: List[Point],
      directionsToConsider: List[Direction8]
  ): (Long, List[Point]) = {
    if (currentRound > totalRounds) {
      currentRound - 1 -> elves
    } else {
      debug(s"In doRound ${currentRound} of ${totalRounds}, directions: ${directionsToConsider.mkString(",")}")
      printElves(elves)

      val elvesSet = elves.toSet

      def allFree(positions: Seq[Point]): Boolean = elvesSet.intersect(positions.toSet).isEmpty

      // first half: propositions
      val proposedNextMoves: List[(Point, Option[Point])] = elves.map { elf =>
        val adjacents = elf.adjacents8()
        if (allFree(adjacents)) {
          // do nothing
          elf -> None
        } else {
          val allPropositions = directionsToConsider.flatMap {
            case North => Option.when(allFree(elf.adjacents8(List(North, NorthEast, NorthWest))))(elf.move8(North))
            case West  => Option.when(allFree(elf.adjacents8(List(West, NorthWest, SouthWest))))(elf.move8(West))
            case South => Option.when(allFree(elf.adjacents8(List(South, SouthEast, SouthWest))))(elf.move8(South))
            case East  => Option.when(allFree(elf.adjacents8(List(East, NorthEast, SouthEast))))(elf.move8(East))
          }
          debug(s"all propositions for ${elf} = ${allPropositions}")
          elf -> allPropositions.headOption
        }
      }

      debug(s"Propositions ${proposedNextMoves.mkString(",")}")

      // second half, actually move
      val elvesThatClash: Set[Point] =
        proposedNextMoves
          .collect { case (elf, Some(dest)) => elf -> dest }
          .groupBy(_._2)
          .collect { case (_, elves) if elves.size > 1 => elves.map(_._1) }
          .flatten
          .toSet

      val filteredPropositions = proposedNextMoves.map { case (elf, proposition) =>
        if (elvesThatClash.contains(elf)) elf -> None
        else elf -> proposition
      }

      if (filteredPropositions.forall(_._2.isEmpty)) {
        debug(s"No more moves @ round $currentRound.")
        currentRound -> elves
      } else {
        //      debug(s"Elves that would clash ${elvesThatClash.mkString(",")}")
        val movedElves: List[Point] = filteredPropositions.map { case (elf, nextMove) =>
          nextMove.getOrElse(elf)
        }
        doRoundRec(totalRounds, currentRound + 1, movedElves, directionsToConsider.tail :+ directionsToConsider.head)
      }
    }
  }

  def parseInput(suffix: Option[String] = None): List[Point] = {
    readLines(suffix).toList.reverse.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (char, x) =>
        Option.when(char == '#')(Point(x, y))
      }
    }.toList
  }

  part1 // 4114
  part2 // 970
}
