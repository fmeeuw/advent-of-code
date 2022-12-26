package util

import util.Direction4.*
import util.Direction8.{NorthEast, NorthWest, SouthEast, SouthWest}
case class Point(x: Int, y: Int) {
  def +(other: Point) = copy(x + other.x, y + other.y)
  def north(steps: Int = 1): Point = copy(y = y + steps)
  def east(steps: Int = 1): Point = copy(x = x + steps)
  def south(steps: Int = 1): Point = copy(y = y - steps)
  def west(steps: Int = 1): Point = copy(x = x - steps)

  def adjacents4(directions: Seq[Direction4] = Direction4.values): List[Point] = directions.map(move4(_)).toList
  def adjacents8(directions: Seq[Direction8] = Direction8.values): List[Point] = directions.map(move8(_)).toList

  def move4(direction: Direction4, steps: Int = 1) = direction match
    case North => north(steps)
    case East  => east(steps)
    case South => south(steps)
    case West  => west(steps)

  def move8(direction: Direction8, steps: Int = 1) = direction match
    case Direction8.North => north(steps)
    case NorthEast        => north(steps).east(steps)
    case Direction8.East  => east(steps)
    case SouthEast        => south(steps).east(steps)
    case Direction8.South => south(steps)
    case SouthWest        => south(steps).west(steps)
    case Direction8.West  => west(steps)
    case NorthWest        => north(steps).west(steps)

}
