package util

import util.Direction.{Down, Up}

case class Point(x: Int, y: Int) {
  def +(other: Point) = copy(x + other.x, y + other.y)
  def up(steps: Int = 1): Point = copy(y = y + steps)
  def right(steps: Int = 1): Point = copy(x = x + steps)
  def down(steps: Int = 1): Point = copy(y = y - steps)
  def left(steps: Int = 1): Point = copy(x = x - steps)

  def move(direction: Direction, steps: Int = 1) = direction match
    case Up              => up(steps)
    case Direction.Right => right(steps)
    case Down            => down(steps)
    case Direction.Left  => left(steps)
}
