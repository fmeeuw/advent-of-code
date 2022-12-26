package aoc2022

import util.{AocApp, Direction4, Grid, Point}
import util.Direction4.*
object Day17 extends AocApp {

  case class Rock(points: Set[Point]) {
    def moveTo(otherPoint: Point): Rock = {
      copy(points = points.map(_ + otherPoint))
    }
    def move(direction: Direction4): Rock = {
      copy(points = points.map(_.move4(direction)))
    }
  }

  // Point 0,0 = bottom left
  val rocks = List(
    /** ####
      */
    Rock(Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0))),
    /** .#. ### .#.
      */
    Rock(Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2))),
    /** ..# ..# ###
      */
    Rock(Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2))),

    /** # # # #
      */
    Rock(Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3))),

    /** ## ##
      */
    Rock(Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1)))
  )

  def part1 = {
    val jetStreamPattern = parseJetStreamPattern()
    println(jetStreamPattern.mkString(","))

    val (endHeight, endState) = moveRockRec(0, 0, 1497, rocks, jetStreamPattern, 3, 3433, Set.empty, Map.empty)
//    printPositions(endState, None)
    println(endHeight)
  }

  def part2 = {
    val jetStreamPattern = parseJetStreamPattern()
    println(jetStreamPattern.mkString(","))
//    val (endHeight, endSkyline) = moveRockRec(0, 0, 5000000, rocks, jetStreamPattern, 0, 0, Set.empty, Map.empty)

    // 1000000000000 rounds to simulate.
    // Cycle with rockIndex == 2 && jetIndex == 3433
    // round=583, totalHeightNow=937
    // Now: round = 2343 , totalHeightNow=3674
    // Now: round = 4103 , totalHeightNow=6411 (diff = 2737)
    // first 583 rounds then 568181817 cycles of 1760 rounds, what remains is 1497 rounds
    // Doing 1497 rounds gives height = 2318 height or perhaps doing one round less 1496 gives 2316, or more will give 2319  (or perhaps 2320)
    // height = (937 + (2737*568181817= 1555113633129)  = 1555113634066)   +  2318 = 1555113636384  /// too high (off by 1)
    // 1555113636385 GOOD OMG

    val (endHeight2, endSkyline2) = moveRockRec(0, 0, 1497, rocks, jetStreamPattern, 3, 3433, Set.empty, Map.empty)

    //    printPositions(endSkyline, None)
    //    println(endHeight)
    println(endHeight2)
  }

  def moveRockRec(
      height: Long,
      round: Long,
      nrOfRounds: Long,
      rocks: List[Rock],
      jetStream: List[Direction4],
      rockIndex: Int,
      jetIndex: Int,
      takenPositions: Set[Point],
      history: Map[(Int, Int), (Long, Option[(Long, Int)])]
  ): (Long, Set[Point]) = {
    if (round == nrOfRounds) {
      (takenPositions.map(_.y).max + height + 1) -> takenPositions
    } else {
      //      println(s"Round ${round} Moving rock ${rockIndex} and jetIndex ${jetIndex} at height=$height.")
      //      println("****************************************************")
      //      printPositions(takenPositions, None)
      //      println("****************************************************")

      val startingPosition = rockStartingPosition(takenPositions.map(_.y).maxOption)
      val rock = rocks(rockIndex).moveTo(startingPosition)
      val (landedRock, updatedJetIndex) = fallRockRec(jetStream, rock, jetTurn = true, jetIndex, takenPositions)
      //      println(s"Rock landed at ${landedRock.points.mkString(",")}")
      val (updatedHeight, updatedTakenPositions) = normalizeSkyLine(height, takenPositions ++ landedRock.points)
      val totalHeightNow: Long = updatedHeight + updatedTakenPositions.map(_.y).max + 1
      val existingHeightDiff = history.get((rockIndex, jetIndex))

      if ((rockIndex == 2 && jetIndex == 3433)) {
        println("My favorite")
        println(s"Now: round = ${round + 1} , totalHeightNow=${totalHeightNow}")
      }

      existingHeightDiff.foreach {
        case (existingHeight, Some((existingHeightDiff, count))) =>
          val newHeightDiff = totalHeightNow - existingHeight
          if (newHeightDiff == existingHeightDiff && count > 3 || rockIndex == 2 && jetIndex == 3433) {
            println(
              s"Detected cycle with count $count in history for rock ${rockIndex} and jetIndex ${jetIndex} at totalHeightNow=$totalHeightNow."
            )
            println(s"History: height:${existingHeight}  heightdiff: ${existingHeightDiff}")
            println(s"Now: round = ${round + 1} HeightDiff = ${newHeightDiff} , totalHeightNow=${totalHeightNow}")
            //          println("****************************************************")
            //          printPositions(takenPositions, None)
            //          println("****************************************************")
          }
        case _ => ()
      }
      moveRockRec(
        updatedHeight,
        round + 1,
        nrOfRounds,
        rocks,
        jetStream,
        (rockIndex + 1) % rocks.size,
        updatedJetIndex,
        updatedTakenPositions,
        history.updated(
          (rockIndex, jetIndex),
          existingHeightDiff
            .map {
              case (existingHeight, Some(diff, count)) =>
                val newHeightDiff = totalHeightNow - existingHeight
                val newCount = if (newHeightDiff == diff) count + 1 else 0
                totalHeightNow -> Some(newHeightDiff -> newCount)
              case (existingHeight, None) =>
                val newHeightDiff = totalHeightNow - existingHeight
                totalHeightNow -> Some(newHeightDiff -> 0)
            }
            .getOrElse(totalHeightNow -> None)
        )
      )
    }
  }

  def normalizeSkyLine(currentHeight: Long, takenPositions: Set[Point]): (Long, Set[Point]) = {
//    val maxY = takenPositions.map(_.y).maxOption.getOrElse(0)
//    val lookback = 120
////    val lowestTop: Int =
////      (0 until 7)
////        .map(x => takenPositions.filter(_.x == x).map(_.y).maxOption.getOrElse(0))
////        .minOption
////        .map(_ - 20) // DUnno why? /cry
////        .getOrElse(0)
//
//    // Now lowestTop will be 0.
////    val normalizedSkyline = takenPositions.filter(point => point.y >= lowestTop).map(point => point.down(lowestTop))
//
////    val normalizedSkyLine2 = (0 until 7).flatMap(x => takenPositions.filter(_.x == x).maxByOption(_.y)).toSet
//
//    val normalizedSkyline = takenPositions.filter(_.y >= maxY - lookback).map(point => point.down(maxY - lookback))
//
//    val newHeight = currentHeight + maxY - lookback
//    newHeight -> normalizedSkyline
    0L -> takenPositions
  }

  def filterTopRows(points: Set[Point], top: Int): Set[Point] = {
    val maxY = points.map(_.y).maxOption.getOrElse(0)
    points.filter(_.y >= maxY - top)
  }

  def fallRockRec(
      jetStream: List[Direction4],
      rock: Rock,
      jetTurn: Boolean,
      jetIndex: Int,
      takenPositions: Set[Point]
  ): (Rock, Int) = {
//    println(s"Falling Rock recursively jetIndex ${jetIndex} and jetTurn=$jetTurn")
//    println("****************************************************")
//    printPositions(takenPositions, Some(rock))
//    println("****************************************************")

    if (jetTurn) {
      val jetDirection = jetStream(jetIndex)
      val rockMovedByJet = rock.move(jetDirection)
      if (isValidRockPosition(rockMovedByJet, takenPositions)) {
        fallRockRec(jetStream, rockMovedByJet, !jetTurn, (jetIndex + 1) % jetStream.size, takenPositions)
      } else {
        fallRockRec(jetStream, rock, !jetTurn, (jetIndex + 1) % jetStream.size, takenPositions)
      }
    } else {
      val rockMovedDown = rock.move(South)
      if (isValidRockPosition(rockMovedDown, takenPositions)) {
        fallRockRec(jetStream, rockMovedDown, !jetTurn, jetIndex, takenPositions)
      } else {
        if (rockMovedDown.points.forall(xWithinBounds)) { // Means it hits the bottom or another rock.
          rock -> jetIndex
        } else {
          fallRockRec(jetStream, rock, !jetTurn, jetIndex, takenPositions)
        }
      }
    }
  }

  def isValidRockPosition(rock: Rock, takenPositions: Set[Point]) = {
    rock.points.forall(point => isFreePoint(point, takenPositions))
  }
  def isFreePoint(point: Point, takenPositions: Set[Point]) = {
    val withinBounds = yWithinBounds(point) && xWithinBounds(point)
    withinBounds && !takenPositions.contains(point)
  }
  def xWithinBounds(point: Point) = point.x >= 0 && point.x < 7
  def yWithinBounds(point: Point) = point.y >= 0

  private def rockStartingPosition(highestRockY: Option[Int]): Point = {
    val x = 2
    val y = highestRockY.getOrElse(-1) + 4
    Point(x, y)
  }

  def parseJetStreamPattern(suffix: Option[String] = None): List[Direction4] = {
    readLines(suffix).toList.head.map { char =>
      char match
        case '<' => West
        case '>' => East
    }.toList
  }

  def printPositions(takenPositions: Set[Point], rock: Option[Rock]) = {
    val maxY = (takenPositions ++ rock.map(_.points).getOrElse(Set.empty[Point])).map(_.y).maxOption.getOrElse(6)
    val minY = (takenPositions ++ rock.map(_.points).getOrElse(Set.empty[Point])).map(_.y).minOption.getOrElse(0)
    val lines = for { y <- minY to maxY } yield {
      s"y=$y  \t\t" ++ (0 until 7)
        .map { x =>
          if (takenPositions.contains(Point(x, y))) '#'
          else if (rock.map(_.points).getOrElse(Set.empty[Point]).contains(Point(x, y))) '@'
          else '.'
        }
        .toString()
    }
    lines.reverse.foreach(println)

  }

  part1 // 3175  (test = 3068)
  part2 // 1555113636385
}
