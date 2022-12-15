package util

/** @param slope
  *   Also called gradient or just `m`
  * @param yIntercept
  *   Also called just `b`
  */
case class IntLineEquation(slope: Int, yIntercept: Int) {

  def intersects(otherLine: IntLineEquation): Option[Point] = {
    if (slope == otherLine.slope) None
    else {
      // Rounds down the x, might go wrong if two lines intersect halfway
      val x = (otherLine.yIntercept - yIntercept) / (slope - otherLine.slope)
      val y = slope * x + yIntercept
      Some(Point(x, y))
    }
  }

}
object IntLineEquation {
  def fromTwoPoints(start: Point, end: Point): Option[IntLineEquation] = {
    val xDiff = end.x - start.x
    val yDiff = end.y - start.y
    // Vertical or horizonal line
    if (xDiff == 0 || yDiff == 0) {
      None
    } else {
      val m = yDiff / xDiff // slope
      val b = start.y - (m * start.x)
      Some(IntLineEquation(m, b))
    }
  }
}
