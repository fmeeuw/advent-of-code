package util

case class Grid[A](cells: Vector[Vector[A]]) {

  override def toString: String = {
    cells.zipWithIndex.reverse //for now reversed
      .map { case (row, y) =>
        y + "\t" + row.map(_.toString).mkString
      }
      .mkString("\n")
  }

  def cellOpt(point: Point): Option[A] =
    if (withinBounds(point)) Some(cell(point)) else None

  def cell(point: Point): A = cells(point.y)(point.x)

  def withinBounds(point: Point): Boolean =
    point.y >= 0 && point.y < cells.size &&
      point.x >= 0 && point.x < cells(point.y).size

  def height: Int = cells.size

  def width: Int = cells(0).size

  def points: Seq[Point] = {
    for {
      y <- 0 until cells.size
      x <- 0 until cells(y).size
    } yield Point(x, y)
  }

  def cellsWithPoints: Vector[Vector[(Point, A)]] = {
    cells.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        Point(x, y) -> cell
      }
    }
  }

  def mapCells[B](f: (Point, A) => B): Grid[B] = {
    copy(cells = cells.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        f(Point(x, y), cell)
      }
    })
  }

  def updated(point: Point, value: A): Grid[A] = {
    copy(cells = cells.updated(point.y, cells(point.y).updated(point.x, value)))
  }

  def diagonals: List[Point] = {
    List(
      Point(0, 1), // up
      Point(1, 1), // upright
      Point(1, 0), // right
      Point(1, -1), // downright
      Point(0, -1), // down
      Point(-1, -1), // downleft
      Point(-1, 0), // left
      Point(-1, 1) // upleft
    )
  }

  def adjacent4Points(point: Point): List[Point] = {
    List(
      Point(0, 1), // up
      Point(1, 0), // right
      Point(0, -1), // down
      Point(-1, 0) // left
    ).map(_ + point)
      .filter(withinBounds)
  }

  def adjacents(point: Point): List[A] = {
    diagonals
      .map(_ + point)
      .filter(point => withinBounds(point))
      .map(point => cell(point))
  }

}
