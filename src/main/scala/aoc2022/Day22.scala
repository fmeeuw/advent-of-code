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

  case class CubeCoord(face: Int, pos: Point)

  case class Cube(internalGrid: Grid[Char], size: Int) {

    val faceOrigins =
      if (size == 4) { // test
        List(
          Point(2, 2),
          Point(0, 1),
          Point(1, 1),
          Point(2, 1),
          Point(2, 0),
          Point(3, 0)
        ).map(_ * size)
      } else {
        List(
          Point(0, 0),
          Point(0, 1),
          Point(1, 1),
          Point(1, 2),
          Point(1, 3),
          Point(2, 3)
        ).map(_ * size)
      }

    def toCubeCoord(point: Point): CubeCoord = {
      val face = getFace(point)
      CubeCoord(face, point - faceOrigins(face - 1))
    }

    def toCoord(cubeCoord: CubeCoord): Point = {
      cubeCoord.pos + faceOrigins(cubeCoord.face - 1)
    }

    def step(point: Point, direction: Direction4): (Point, Direction4) = {
      val nextPos = point.move4(direction, 1)
      internalGrid.cellOpt(nextPos) match
        case Some(value) if value == '#'      => nextPos -> direction
        case Some(value) if value == '.'      => nextPos -> direction
        case Some(value) if value.isSpaceChar => toAdjacentFace(point, direction)
        case None                             => toAdjacentFace(point, direction)
    }

    def getFace(point: Point): Int = {
      faceOrigins.zipWithIndex.collectFirst {
        case (origin, idx)
            if point.x >= origin.x && point.x < (origin.x + size) && point.y >= origin.y && point.y < (origin.y + size) =>
          idx + 1
      }.get
    }

    def flipFrom(point: Point, from: Direction4, to: Direction4) = {
      def inverse(value: Int) = size - value - 1
      (from, to) match
        case (North, North) | (South, South) => Point(inverse(point.x), point.y)
        case (East, East) | (West, West)     => Point(point.x, inverse(point.y))

        case (North, South) | (South, North) => Point(point.x, inverse(point.y))
        case (East, West) | (West, East)     => Point(inverse(point.x), point.y)

        case (North, East) | (South, West) => Point(point.y, point.x)
        case (East, North) | (West, South) => Point(point.y, point.x)

        case (East, South) | (West, North) => Point(inverse(point.y), inverse(point.x))
        case (North, West) | (South, East) => Point(inverse(point.y), inverse(point.x))
    }

    def toAdjacentFace(point: Point, direction: Direction4): (Point, Direction4) = {
      val sourceCubeCoord = toCubeCoord(point)
      val (targetCubeCoord, targetDirection) = toAdjacentFaceCubeCoords(sourceCubeCoord, direction)
      debug(
        s"In toAdjacentFace for point ${point} and direction ${direction}, which is cubeCoord: ${sourceCubeCoord}, targetCoord = ${targetCubeCoord} and targetDirection=${targetDirection}."
      )
      toCoord(targetCubeCoord) -> targetDirection
    }

    def toAdjacentFaceCubeCoords(cubeCoord: CubeCoord, direction: Direction4): (CubeCoord, Direction4) = {
      if (size == 4) { // Test
        (cubeCoord.face -> direction) match
          case (1, North) => CubeCoord(2, flipFrom(cubeCoord.pos, North, North)) -> South
          case (1, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, East)) -> West
          case (1, South) => CubeCoord(4, flipFrom(cubeCoord.pos, South, North)) -> South
          case (1, West)  => CubeCoord(3, flipFrom(cubeCoord.pos, West, North)) -> South

          case (2, North) => CubeCoord(1, flipFrom(cubeCoord.pos, North, North)) -> South
          case (2, East)  => CubeCoord(3, flipFrom(cubeCoord.pos, East, West)) -> East
          case (2, South) => CubeCoord(5, flipFrom(cubeCoord.pos, South, South)) -> North
          case (2, West)  => CubeCoord(6, flipFrom(cubeCoord.pos, West, South)) -> North

          case (3, North) => CubeCoord(1, flipFrom(cubeCoord.pos, North, West)) -> East
          case (3, East)  => CubeCoord(4, flipFrom(cubeCoord.pos, East, West)) -> East
          case (3, South) => CubeCoord(5, flipFrom(cubeCoord.pos, South, West)) -> East
          case (3, West)  => CubeCoord(2, flipFrom(cubeCoord.pos, West, East)) -> West

          case (4, North) => CubeCoord(1, flipFrom(cubeCoord.pos, North, South)) -> North
          case (4, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, North)) -> South
          case (4, South) => CubeCoord(5, flipFrom(cubeCoord.pos, South, North)) -> South
          case (4, West)  => CubeCoord(3, flipFrom(cubeCoord.pos, West, East)) -> West

          case (5, North) => CubeCoord(4, flipFrom(cubeCoord.pos, North, South)) -> North
          case (5, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, West)) -> East
          case (5, South) => CubeCoord(2, flipFrom(cubeCoord.pos, South, South)) -> North
          case (5, West)  => CubeCoord(3, flipFrom(cubeCoord.pos, West, South)) -> North

          case (6, North) => CubeCoord(4, flipFrom(cubeCoord.pos, North, East)) -> West
          case (6, East)  => CubeCoord(1, flipFrom(cubeCoord.pos, East, East)) -> West
          case (6, South) => CubeCoord(2, flipFrom(cubeCoord.pos, South, West)) -> East
          case (6, West)  => CubeCoord(5, flipFrom(cubeCoord.pos, West, East)) -> West
      } else { // Real
        (cubeCoord.face -> direction) match
          case (1, North) => CubeCoord(2, flipFrom(cubeCoord.pos, North, South)) -> North
          case (1, East)  => CubeCoord(3, flipFrom(cubeCoord.pos, East, South)) -> North
          case (1, South) => CubeCoord(6, flipFrom(cubeCoord.pos, South, North)) -> South
          case (1, West)  => CubeCoord(5, flipFrom(cubeCoord.pos, West, North)) -> South

          case (2, North) => CubeCoord(4, flipFrom(cubeCoord.pos, North, West)) -> East
          case (2, East)  => CubeCoord(3, flipFrom(cubeCoord.pos, East, West)) -> East
          case (2, South) => CubeCoord(1, flipFrom(cubeCoord.pos, South, North)) -> South
          case (2, West)  => CubeCoord(5, flipFrom(cubeCoord.pos, West, West)) -> East

          case (3, North) => CubeCoord(4, flipFrom(cubeCoord.pos, North, South)) -> North
          case (3, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, East)) -> West
          case (3, South) => CubeCoord(1, flipFrom(cubeCoord.pos, South, East)) -> West
          case (3, West)  => CubeCoord(2, flipFrom(cubeCoord.pos, West, East)) -> West

          case (4, North) => CubeCoord(5, flipFrom(cubeCoord.pos, North, South)) -> North
          case (4, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, South)) -> North
          case (4, South) => CubeCoord(3, flipFrom(cubeCoord.pos, South, North)) -> South
          case (4, West)  => CubeCoord(2, flipFrom(cubeCoord.pos, West, North)) -> South

          case (5, North) => CubeCoord(1, flipFrom(cubeCoord.pos, North, West)) -> East
          case (5, East)  => CubeCoord(6, flipFrom(cubeCoord.pos, East, West)) -> East
          case (5, South) => CubeCoord(4, flipFrom(cubeCoord.pos, South, North)) -> South
          case (5, West)  => CubeCoord(2, flipFrom(cubeCoord.pos, West, West)) -> East

          case (6, North) => CubeCoord(1, flipFrom(cubeCoord.pos, North, South)) -> North
          case (6, East)  => CubeCoord(3, flipFrom(cubeCoord.pos, East, East)) -> West
          case (6, South) => CubeCoord(4, flipFrom(cubeCoord.pos, South, East)) -> West
          case (6, West)  => CubeCoord(5, flipFrom(cubeCoord.pos, West, East)) -> West
      }
    }
  }
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

  def part2 = {
    val Input(grid, instructions) = parseInput()
    val startPoint = grid.cellsWithPoints.last.collectFirst { case (point, char) if char == '.' => point }.get
    val startDirection = East
    val cube = Cube(grid, 50)

    debug(grid)
    debug(s"Starting pos ${startPoint} in direction ${startDirection}")
    val endState = instructions.foldLeft(State(startPoint, startDirection, List.empty)) {
      case (state @ State(pos, direction, visited), instruction) =>
        debug(
          s"Performing instruction ${instruction}, current pos ${pos}, dir: $direction, visited: ${visited.size}."
        )
        instruction match
          case MoveForward(steps) =>
            val (nextPoint, nextDirection) = move2(cube, pos, direction, steps)
            state.copy(nextPoint, nextDirection, pos :: visited)
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

    val row = grid.height - endState.pos.y
    val column = endState.pos.x + 1
    val result = (1000 * row) + (4 * column) + facingValue
    debug(
      s"password = 1000 * ${row} + 4 * ${column} + ${facingValue} = ${result}"
    )
    info(result)

    // 16212 too low!
  }

  def move2(cube: Cube, point: Point, direction: Direction4, steps: Int): (Point, Direction4) = {
    debug(s"Moving steps ${steps} in direction ${direction} from pos ${point}.")
    if (steps == 0) point -> direction
    else {
      val (nextPos, nextDirection) = cube.step(point, direction)
      val char = cube.internalGrid.cell(nextPos)
      debug(s"Next pos = ${nextPos} , with char  ${char}.")
      if (char == '#') {
        point -> direction
      } else {
        move2(cube, nextPos, nextDirection, steps - 1)
      }
    }
  }

  def parseInput(suffix: Option[String] = None): Input = {
    val lines = readLines(suffix).toList
    val gridLines = lines.takeWhile(_.nonEmpty).reverse
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

  part1 // 79248
  part2 // 156166

}
