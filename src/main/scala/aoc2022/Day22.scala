package aoc2022

import util.{AocApp, Direction4, Grid, Point}
import util.Direction4.*
object Day22 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Input(grid: Grid[Char], instructions: List[Instruction])

  sealed trait Instruction
  case class MoveForward(steps: Int) extends Instruction
  case class Rotate(clockwise: Boolean) extends Instruction

  case class State(pos: Point, direction: Direction4, visited: List[Point])
  def part1 = {
    val Input(grid, instructions) = parseInput()
    val startPoint = grid.cellsWithPoints.head.collectFirst { case (point, char) if char == '.' => point }.get
    val startDirection = East

    debug(grid)
    debug(s"Starting pos ${startPoint} in direction ${startDirection}")

    val endState = instructions.foldLeft(State(startPoint, startDirection, List.empty)) {
      case (state @ State(pos, direction, visited), instruction) =>
        debug(
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

    debug(s"Endstate,  pos ${endState.pos}, dir: ${endState.direction}, visited: ${endState.visited}.")
    val facingValue: Int = endState.direction match
      case North => 3
      case East  => 0
      case South => 1
      case West  => 2

    val result = (1000 * (endState.pos.y + 1)) + (4 * (endState.pos.x + 1)) + facingValue
    debug(
      s"password = 1000 * ${endState.pos.y + 1} + 4 * ${endState.pos.x + 1} + ${facingValue} = ${result}"
    )
    info(result)
  }

  def move(grid: Grid[Char], point: Point, direction: Direction4, steps: Int): Point = {
    debug(s"Moving steps ${steps} in direction ${direction} from pos ${point}.")
    if (steps == 0) point
    else {
      val nextFree = nextFreeSpace(grid, point, direction)
      debug(s"Next free = ${nextFree}.")
      nextFree match
        case Some(value) => move(grid, value, direction, steps - 1)
        case None        => point
    }
  }

  def nextFreeSpace(grid: Grid[Char], point: Point, direction: Direction4): Option[Point] = {
    val nextPos = point.move4(direction.inverseNorthSouth, 1)
    debug(
      s"In nextFreeSpace ${point}, direction=${direction}' , nextPos = ${nextPos}, nextCell = ${grid.cellOpt(nextPos)}"
    )
    grid.cellOpt(nextPos) match
      case Some(value) if value == '#'      => None
      case Some(value) if value == '.'      => Some(nextPos)
      case Some(value) if value.isSpaceChar => nextFreeSpace(grid, nextPos, direction)
      case None =>
        val newPoint = direction match
          case North => point.copy(y = grid.height)
          case East  => point.copy(x = -1)
          case South => point.copy(y = -1)
          case West  => point.copy(x = grid.width - 1)
        nextFreeSpace(grid, newPoint, direction)
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
