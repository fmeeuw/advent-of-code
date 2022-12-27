package aoc2022

import util.{AocApp, Point}

object Day18 extends AocApp {

  override val logOnDebug: Boolean = false
  case class State(cubes: Set[Point3], reachable: Set[Point3], unrachable: Set[Point3]) {

    def contains(point: Point3): Boolean = {
      cubes.contains(point) || reachable.contains(point) || unrachable.contains(point)
    }

    def withinBounds(point: Point3): Boolean = {
      point.x >= cubes.map(_.x).min - 3 && point.x <= cubes.map(_.x).max + 3 &&
      point.y >= cubes.map(_.y).min - 3 && point.y <= cubes.map(_.y).max + 3 &&
      point.z >= cubes.map(_.z).min - 3 && point.z <= cubes.map(_.z).max + 3

    }
    def isReachableFromOrigin(point: Point3): Option[Boolean] = {
      if (reachable.contains(point)) {
        Some(true)
      } else if (unrachable.contains(point) || cubes.contains(point)) {
        Some(false)
      } else {
        None
      }
    }
  }

  case class Point3(x: Int, y: Int, z: Int) {
    def up(steps: Int = 1): Point3 = copy(y = y + steps)

    def right(steps: Int = 1): Point3 = copy(x = x + steps)

    def down(steps: Int = 1): Point3 = copy(y = y - steps)

    def left(steps: Int = 1): Point3 = copy(x = x - steps)

    def forward(steps: Int = 1): Point3 = copy(z = z - steps)
    def backward(steps: Int = 1): Point3 = copy(z = z + steps)

  }

  def part1 = {
    val points = parsePoints().toSet
    debug(points.mkString(","))

    debug(surfaceArea(Set(Point3(1, 1, 1), Point3(2, 1, 1))))
    info(surfaceArea(points))
  }

  def part2 = {
//    val points = Set(Point3(1, 1, 1), Point3(2, 1, 1))
    val points = parsePoints().toSet
    debug(points.mkString(","))

    val reachablePoints = fillRec(List(Point3(0, 0, 0)), points, Set.empty)
    info(totalSurfaceArea(points, reachablePoints))
  }

  def totalSurfaceArea(cubes: Set[Point3], reachablePoints: Set[Point3]): Int = {
    cubes.toList.map(point => adjacents(point).count(reachablePoints.contains)).sum
  }

  def countReachableAdjacents(adjacents: List[Point3], state: State, count: Int): (State, Int) = {
    adjacents match
      case ::(head, next) =>
        val (updatedState, isReachable) = reachableFromOrigin(head, state)
        val updatedCount = if (isReachable) count + 1 else count
        countReachableAdjacents(next, updatedState, updatedCount)
      case Nil => state -> count
  }

  def surfaceArea(points: Set[Point3]): Int = {
    points.toList.map(point => 6 - adjacents(point).count(points.contains)).sum
  }

  def fillRec(todo: List[Point3], stones: Set[Point3], visited: Set[Point3]): Set[Point3] = {
    def tryNext(point: Point3) = {
      !stones.contains(point) && point.x >= stones.map(_.x).min - 3 && point.x <= stones.map(_.x).max + 3 &&
      point.y >= stones.map(_.y).min - 3 && point.y <= stones.map(_.y).max + 3 &&
      point.z >= stones.map(_.z).min - 3 && point.z <= stones.map(_.z).max + 3
    }

    todo match
      case ::(point, next) =>
        if (stones.contains(point)) {
          fillRec(next, stones, visited)
        } else {
          val updatedTodo = adjacents(point).filter(tryNext).filterNot(visited.contains).filterNot(next.contains)
          fillRec(next ++ updatedTodo, stones, visited + point)
        }
      case Nil => visited
  }

  def reachableFromOrigin(point: Point3, state: State): (State, Boolean) = {
    debug(s"In reachableFromOrigin ${point} state ${state}")
    state.isReachableFromOrigin(point) match
      case Some(reachable) => state -> reachable
      case None if (!state.withinBounds(point)) =>
        state.copy(reachable = state.reachable + point) -> true
      case None if (state.withinBounds(point)) =>
        val (updatedState, anyReachable) = findAnyReachable(adjacents(point), state)
        if (anyReachable) {
          updatedState.copy(reachable = updatedState.reachable + point) -> true
        } else {
          updatedState.copy(unrachable = updatedState.unrachable + point) -> false
        }
  }

  def findAnyReachable(points: List[Point3], state: State): (State, Boolean) = {
    points match
      case ::(head, next) =>
        val (updatedState, isReachable) = reachableFromOrigin(head, state)
        if (isReachable) state -> true
        else findAnyReachable(next, updatedState)
      case Nil => state -> false
  }

  def adjacents(point: Point3): List[Point3] = {
    List(
      point.up(),
      point.down(),
      point.left(),
      point.right(),
      point.forward(),
      point.backward()
    )
  }

  def parsePoints(suffix: Option[String] = None): List[Point3] = {
    readLines(suffix).toList.map { (line: String) =>
      line.split(",").toList match {
        case List(x, y, z) => Point3(x.toInt, y.toInt, z.toInt)
      }
    }
  }

//  part1 // 4444
  part2
}
