package aoc2022

import util.{AocApp, Point}
object Day15 extends AocApp {

  override val logOnDebug: Boolean = false
  case class Input(sensor: Point, beacon: Point) {
    lazy val distanceSensorBeacon = manhattanDistance(sensor, beacon)
  }

  def part1 = {
    val input = parseInput()
    debug(input.mkString("\n"))
    val filterForY = 2000000
    val allConfirmedNoBeaconsForY: Set[Point] = input.flatMap(input => confirmedNoBeacons(input, filterForY)).toSet
    val allConfirmedBeacons: Set[Point] = input.map(_.beacon).toSet
    val result = allConfirmedNoBeaconsForY.size - allConfirmedBeacons.count(_.y == filterForY)
    info(result)
  }

  def part2 = {
    val maxSearchY = 4000000
    val input = parseInput()
    val edges: Set[Point] = input.flatMap(calcEdges).toSet
    val onlyFreePoints = edges
      .filter(edge => input.forall(input => outSideRange(edge, input)))
      .filter(point => point.x >= 0 && point.x <= maxSearchY && point.y >= 0 && point.y <= maxSearchY)

    info(tuningFrequency(onlyFreePoints.head))
  }

  def calcEdges(input: Input): Set[Point] = {
    val left = Point(input.sensor.x - input.distanceSensorBeacon - 1, input.sensor.y)
    val right = Point(input.sensor.x + input.distanceSensorBeacon + 1, input.sensor.y)
    (for { i <- 0 to input.distanceSensorBeacon + 1 } yield {
      Set(left.north(i).east(i), left.south(i).east(i), right.north(i).west(i), left.south(i).west(i))
    }).flatten.toSet
  }

  def outSideRange(point: Point, input: Input): Boolean = {
    manhattanDistance(input.sensor, point) > input.distanceSensorBeacon
  }

  def tuningFrequency(point: Point): Long = {
    point.x * 4000000L + point.y
  }

  def confirmedNoBeacons(input: Input, filterForY: Int): Set[Point] = {
    val distance = manhattanDistance(input.beacon, input.sensor)
    debug(s"Detecting no beacon area for input $input with manhattan distance $distance filtered for y=${filterForY}")
    val result = (for {
      x <- input.sensor.x - distance to input.sensor.x + distance
      y = filterForY // input.sensor.y - distance to input.sensor.y + distance
      if manhattanDistance(input.sensor, Point(x, y)) <= distance
    } yield Point(x, y)).toSet
    debug(result.mkString(","))
    result
  }

  def parseInput(suffixOpt: Option[String] = None) = {
    val regex = ".*x=(-?\\d+).*y=(-?\\d+).*x=(-?\\d+).*y=(-?\\d+).*".r
    readLines(suffixOpt).toList.map { line =>
      val hit = regex.findFirstMatchIn(line).get
      Input(
        sensor = Point(hit.group(1).toInt, hit.group(2).toInt),
        beacon = Point(hit.group(3).toInt, hit.group(4).toInt)
      )
    }
  }

  def manhattanDistance(a: Point, b: Point): Int = {
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }

  part1 // 5147333
  part2 // 13734006908372
}
