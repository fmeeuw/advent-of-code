package aoc2020

import util.InputOps

object Day12 extends App {

  case class Action(action: Char, amount: Int)
  case class Point(x: Int, y: Int)

  case class State1(position: Point, degrees: Int)
  case class State2(ship: Point, waypoint: Point)

  def part1 = {
    val actions = parseInput
    val beginState = State1(Point(0, 0), 90)
    val endState = actions.foldLeft(beginState) { case (state, action) =>
      val nextState = performAction1(state, action)
//      println(
//        s"In state $state, performing action $action, next state $nextState"
//      )
      nextState
    }
//    println(endState)
    println(manhattanDistance(endState.position, beginState.position))
  }

  def part2 = {
    val actions = parseInput
    val beginState = State2(Point(0, 0), Point(10, 1))
    val endState = actions.foldLeft(beginState) { case (state, action) =>
      val nextState = performAction2(state, action)
//      println(
//        s"In state $state, performing action $action, next state $nextState"
//      )
      nextState
    }
//    println(endState)
    println(manhattanDistance(endState.ship, beginState.ship))
  }

  def parseInput: Vector[Action] =
    InputOps
      .readLines(2020, 12)
      .map { line =>
        val (action, amount) = line.splitAt(1)
        Action(action.charAt(0), amount.toInt)
      }
      .toVector

  def manhattanDistance(a: Point, b: Point): Int = {
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }

  def performAction1(state: State1, action: Action): State1 = {
    val State1(Point(x, y), degrees: Int) = state

    action match {
      case Action('N', amount) =>
        state.copy(position = Point(x, y + amount))
      case Action('S', amount) =>
        state.copy(position = Point(x, y - amount))
      case Action('E', amount) =>
        state.copy(position = Point(x + amount, y))
      case Action('W', amount) =>
        state.copy(position = Point(x - amount, y))
      case Action('L', amount) =>
        state.copy(degrees = addDegrees(degrees, amount * -1))
      case Action('R', amount) =>
        state.copy(degrees = addDegrees(degrees, amount))
      case Action('F', amount) =>
        degrees match {
          case 0   => performAction1(state, Action('N', amount))
          case 90  => performAction1(state, Action('E', amount))
          case 180 => performAction1(state, Action('S', amount))
          case 270 => performAction1(state, Action('W', amount))
        }
    }

  }

  def addDegrees(current: Int, next: Int): Int = {
    val newDegrees = current + next % 360
    val roundedDegrees = {
      if (newDegrees < 0) newDegrees + 360
      else if (newDegrees >= 360) newDegrees - 360
      else newDegrees
    }
    roundedDegrees
  }

  def performAction2(state: State2, action: Action): State2 = {
    val State2(ship, waypoint) = state

    // The waypoint stays 10 units east and 4 units north of the ship.
    // After R90
    // 4 units east and 10 units south
    def rotateWaypoint(degree: Int): Point = {
      val newDegrees = addDegrees(0, degree)
      newDegrees match {
        case 0   => waypoint
        case 90  => Point(waypoint.y, -waypoint.x)
        case 180 => Point(-waypoint.x, -waypoint.y)
        case 270 => Point(-waypoint.y, +waypoint.x)
      }
    }

    action match {
      case Action('N', amount) =>
        state.copy(waypoint = Point(waypoint.x, waypoint.y + amount))
      case Action('S', amount) =>
        state.copy(waypoint = Point(waypoint.x, waypoint.y - amount))
      case Action('E', amount) =>
        state.copy(waypoint = Point(waypoint.x + amount, waypoint.y))
      case Action('W', amount) =>
        state.copy(waypoint = Point(waypoint.x - amount, waypoint.y))
      case Action('L', amount) =>
        state.copy(waypoint = rotateWaypoint(amount * -1))
      case Action('R', amount) =>
        state.copy(waypoint = rotateWaypoint(amount))
      case Action('F', amount) =>
        val deltaX = waypoint.x * amount
        val deltaY = waypoint.y * amount
        state.copy(ship = Point(ship.x + deltaX, ship.y + deltaY))
    }

  }

  part1 // 1032
  part2 // 156735

}
