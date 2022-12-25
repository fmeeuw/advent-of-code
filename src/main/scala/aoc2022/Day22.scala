package aoc2022

import util.Direction.{Down, Up}
import util.{AocApp, Direction, Grid, Point}

object Day22 extends AocApp {

  case class Input(grid: Grid[Char], instructions: List[Instruction])

  sealed trait Instruction
  case class MoveForward(steps: Int) extends Instruction
  case class Rotate(clockwise: Boolean) extends Instruction

  case class State(pos: Point, direction: Direction, visited: List[Point])
  def part1 = {
    val Input(grid, instructions) = parseInput()
    val startPoint = grid.cellsWithPoints.head.collectFirst { case (point, char) if char == '.' => point }.get
    val startDirection = Direction.Right

    println(grid)
    println(s"Starting pos ${startPoint} in direction ${startDirection}")

    val endState = instructions.foldLeft(State(startPoint, startDirection, List.empty)) {
      case (state @ State(pos, direction, visited), instruction) =>
        println(
          s"Performing instruction ${instruction}, current pos ${pos}, dir: $direction, visited: ${visited.size}."
        )
        instruction match
          case MoveForward(steps) =>
            val nextPoint = move(grid, pos, direction, steps)
            state.copy(nextPoint, direction, pos :: visited)
          case Rotate(clockwise) =>
            val nextDirection = if (clockwise) direction.rotateClockwise else direction.rotateCounterClockwise
            state.copy(pos, nextDirection, pos :: visited)
    }

    println(s"Endstate,  pos ${endState.pos}, dir: ${endState.direction}, visited: ${endState.visited}.")
    val facingValue: Int = endState.direction match
      case Up              => 3
      case Direction.Right => 0
      case Down            => 1
      case Direction.Left  => 2

    println(
      s"password = 1000 * ${endState.pos.y + 1} + 4 * ${endState.pos.x + 1} + ${facingValue} = ${(1000 * (endState.pos.y + 1)) + (4 * (endState.pos.x + 1)) + facingValue}"
    )
  }

  def move(grid: Grid[Char], point: Point, direction: Direction, steps: Int): Point = {
    println(s"Moving steps ${steps} in direction ${direction} from pos ${point}.")
    if (steps == 0) point
    else {
      val nextFree = nextFreeSpace(grid, point, direction)
      println(s"Next free = ${nextFree}.")
      nextFree match
        case Some(value) => move(grid, value, direction, steps - 1)
        case None        => point
    }
  }

  def nextFreeSpace(grid: Grid[Char], point: Point, direction: Direction): Option[Point] = {
    val nextPos = point.move(inverseUpDown(direction), 1)
    println(
      s"In nextFreeSpace ${point}, direction=${direction}' , nextPos = ${nextPos}, nextCell = ${grid.cellOpt(nextPos)}"
    )
    grid.cellOpt(nextPos) match
      case Some(value) if value == '#'      => None
      case Some(value) if value == '.'      => Some(nextPos)
      case Some(value) if value.isSpaceChar => nextFreeSpace(grid, nextPos, direction)
      case None =>
        val newPoint = direction match
          case Up              => point.copy(y = grid.height)
          case Direction.Right => point.copy(x = -1)
          case Down            => point.copy(y = -1)
          case Direction.Left  => point.copy(x = grid.width - 1)
        nextFreeSpace(grid, newPoint, direction)
  }

  def inverseUpDown(direction: Direction): Direction = {
    direction match
      case Up              => Down
      case Direction.Right => Direction.Right
      case Down            => Up
      case Direction.Left  => Direction.Left
  }

  def parseInput(suffix: Option[String] = None): Input = {
    val lines = readLines(suffix).toList
    val gridLines = lines.takeWhile(_.nonEmpty)
    val instructionLine = lines.last

    val maxLine = gridLines.map(_.length).max

    val grid = Grid(gridLines.map(line => (line ++ " " * (maxLine - line.length)).toVector).toVector)

    val instructions = parseInstruction(instructionLine.toList, List.empty).reverse
    Input(grid, instructions)
  }

  def parseInstruction(line: List[Char], acc: List[Instruction]): List[Instruction] = {
    line match
      case ::(head, next) =>
        if (head.isDigit) {
          val steps = line.takeWhile(_.isDigit).mkString.toInt
          parseInstruction(next.dropWhile(_.isDigit), MoveForward(steps) :: acc)
        } else if (head == 'R') {
          parseInstruction(next, Rotate(true) :: acc)
        } else if (head == 'L') {
          parseInstruction(next, Rotate(false) :: acc)
        } else {
          throw new IllegalStateException("failure")
        }
      case Nil => acc
  }

  part1

}
