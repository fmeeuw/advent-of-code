package util

import util.Direction4.East

enum Direction4 {

  def rotateClockwise: Direction4 = {
    this match
      case North => East
      case East  => South
      case South => West
      case West  => North
  }

  def rotateCounterClockwise: Direction4 = {
    this match
      case North => West
      case East  => North
      case South => East
      case West  => South
  }

  def inverseNorthSouth: Direction4 = {
    this match
      case North => South
      case East  => East
      case South => North
      case West  => West
  }

  case North, East, South, West
}
