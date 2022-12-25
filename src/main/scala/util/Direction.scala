package util

import util.Direction.{Down, Up}

enum Direction:

  def rotateClockwise = {
    this match
      case Up              => Direction.Right
      case Direction.Right => Down
      case Down            => Direction.Left
      case Direction.Left  => Up
  }

  def rotateCounterClockwise = {
    this match
      case Up              => Direction.Left
      case Direction.Right => Up
      case Down            => Direction.Right
      case Direction.Left  => Down
  }

  case Up, Right, Down, Left
