package aoc2022

import util.AocApp

object Day16 extends AocApp {

  case class Valve(name: String, flowRate: Int, opened: Boolean, connectedValves: List[String])

  sealed trait Action
  case class Move(from: String, to: String) extends Action
  case class Open(valve: String) extends Action

  def part1 = {
    val valves: Seq[Valve] = parseInput()
    println(valves.mkString("\n"))
    val valvesMap = valves.map(valve => valve.name -> valve).toMap

    val valvesOfInterst = valves.filter(valve => valve.flowRate > 0 || valve.name == "AA")
    val shortestPathMap: Map[(String, String), Int] = valvesOfInterst
      .combinations(2)
      .toList
      .flatMap(valves =>
        val from = valves(0).name
        val to = valves(1).name
        val distance = shortestPath(to, List(0 -> from), Set.empty, valvesMap)
        println(s"Calculating shortest path $from -> $to = $distance")
        List(
          (valves(0).name, valves(1).name) -> distance,
          (valves(1).name, valves(0).name) -> distance
        )
      )
      .toMap

    val score = moveRec(30, "AA", valvesMap, shortestPathMap, 0)
    println(score)
  }

  def part2 = {
    val valves: Seq[Valve] = parseInput()
    println(valves.mkString("\n"))
    val valvesMap = valves.map(valve => valve.name -> valve).toMap

    val valvesOfInterst = valves.filter(valve => valve.flowRate > 0 || valve.name == "AA")
    val shortestPathMap: Map[(String, String), Int] = valvesOfInterst
      .combinations(2)
      .toList
      .flatMap(valves =>
        val from = valves(0).name
        val to = valves(1).name
        val distance = shortestPath(to, List(0 -> from), Set.empty, valvesMap)
        println(s"Calculating shortest path $from -> $to = $distance")
        List(
          (valves(0).name, valves(1).name) -> distance,
          (valves(1).name, valves(0).name) -> distance
        )
      )
      .toMap

    val score = moveRec2(26, 26, "AA", "AA", valvesMap, shortestPathMap, 0)
    println(score)
  }
  def moveRec(
      minutesLeft: Int,
      currentValve: String,
      valves: Map[String, Valve],
      shortestPathMap: Map[(String, String), Int],
      score: Int
  ): Int = {
    val possibleMoves = valves.values
      .filter(valve => !valve.opened && valve.flowRate > 0)
      .map { valve =>
        val distance = shortestPathMap((currentValve, valve.name))
        val nextMinutesLeft = (minutesLeft - distance - 1)
        val scoreByOpeningValve = nextMinutesLeft * valve.flowRate
        (valve, distance + 1, scoreByOpeningValve)
      }
      .filter(_._3 > 0)
      .toList

//    println(
//      s"Considering nextValve to be ${valve.name} with rate=${valve.flowRate}, minutesLeft=${minutesLeft}  minutesTaken=${minutes} adding score of ${openingScore}"
//    )
    if (possibleMoves.isEmpty) score
    else {
      possibleMoves.map { case (valve, minutes, openingScore) =>
        moveRec(
          minutesLeft = minutesLeft - minutes,
          valve.name,
          valves.updated(valve.name, valve.copy(opened = true)),
          shortestPathMap,
          score = score + openingScore
        )
      }.max
    }
  }

  def moveRec2(
      minutesLeftA: Int,
      minutesLeftB: Int,
      currentValveA: String,
      currentValveB: String,
      valves: Map[String, Valve],
      shortestPathMap: Map[(String, String), Int],
      score: Int
  ): Int = {

    val isA = minutesLeftA >= minutesLeftB
    val minutesLeft = if (isA) minutesLeftA else minutesLeftB
    val currentValve = if (isA) currentValveA else currentValveB

    val possibleMoves = valves.values
      .filter(valve => !valve.opened && valve.flowRate > 0)
      .map { valve =>
        val distance = shortestPathMap((currentValve, valve.name))
        val nextMinutesLeft = (minutesLeft - distance - 1)
        val scoreByOpeningValve = nextMinutesLeft * valve.flowRate
        (valve, distance + 1, scoreByOpeningValve)
      }
      .filter(_._3 > 0)
      .toList

    //    println(
    //      s"Considering nextValve to be ${valve.name} with rate=${valve.flowRate}, minutesLeft=${minutesLeft}  minutesTaken=${minutes} adding score of ${openingScore}"
    //    )
    if (possibleMoves.isEmpty) score
    else {
      possibleMoves.map { case (valve, minutes, openingScore) =>
        if (isA) {
          moveRec2(
            minutesLeftA = minutesLeft - minutes,
            minutesLeftB = minutesLeftB,
            currentValveA = valve.name,
            currentValveB = currentValveB,
            valves.updated(valve.name, valve.copy(opened = true)),
            shortestPathMap,
            score = score + openingScore
          )
        } else {
          moveRec2(
            minutesLeftA = minutesLeftA,
            minutesLeftB = minutesLeft - minutes,
            currentValveA = currentValveA,
            currentValveB = valve.name,
            valves.updated(valve.name, valve.copy(opened = true)),
            shortestPathMap,
            score = score + openingScore
          )
        }
      }.max
    }
  }

  def shortestPath(
      to: String,
      todo: List[(Int, String)],
      visited: Set[String],
      valves: Map[String, Valve]
  ): Int = {
    todo match
      case ::((steps, from), next) =>
        if (from == to) steps
        else {
          val valve = valves(from)
          val newTodo = (valve.connectedValves
            .filterNot(visited.contains)
            .filterNot(valve => todo.exists(_._2 == valve))
            .map(name => steps + 1 -> name) ++ next).sortBy(_._1)
          shortestPath(to, newTodo, visited, valves)
        }
      case Nil => Int.MaxValue
  }

//  def moveRec(
//      minutesLeft: Int,
//      valves: Map[String, Valve],
//      currentValve: String,
//      actions: List[Action]
//  ): List[Action] = {
////    println(s"In moveRec minutesLEft=${minutesLeft}, currentValve: $currentValve, actions: ${actions.mkString(",")}")
//    if (minutesLeft == 0) {
//      actions
//    } else {
//      val valve = valves(currentValve)
//      if (!valve.opened && valve.flowRate > 0) {
//        moveRec(
//          minutesLeft - 1,
//          valves.updated(currentValve, valve.copy(opened = true)),
//          currentValve,
//          Open(currentValve) :: actions
//        )
//      } else {
//        valve.connectedValves
//          .map(nextValve => moveRec(minutesLeft - 1, valves, nextValve, Move(currentValve, nextValve) :: actions))
//          .maxBy(actions => score(actions, valves))
//      }
//    }
//  }

  def score(actions: List[Action], valves: Map[String, Valve]): Int = {
    val totalMinutes = actions.size
    actions.zipWithIndex.collect { case (Open(valve), index) =>
      valves(valve).flowRate * totalMinutes - index
    }.sum
  }

  def parseInput(suffix: Option[String] = None): List[Valve] = {
    readLines(suffix).toList.map { line =>
      val valve = line.slice(6, 8)
      val flowRate = line.drop(23).takeWhile(_.isDigit).toInt
      println(line.split(";").last.drop(23))
      val connectedValves: List[String] = line.split(";").last.drop(23).split(",").map(_.trim).toList
//      println(s"Parsed valve $valve with flowRate $flowRate and connectedValves=${connectedValves.mkString(",")}")
      Valve(valve, flowRate, opened = false, connectedValves)
    }
  }

  part1
  part2
}
