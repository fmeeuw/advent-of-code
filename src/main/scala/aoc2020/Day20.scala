package aoc2020

import util.{AocApp, Grid, Point}

import scala.annotation.tailrec

object Day20 extends AocApp {

  /** Tile 1409: ##..#.#.#. ##........ #.#...##.# #..#..#... .......##. ##......## .......... .........# .#..##....
    * #.##...##.
    */

  case class Image(id: Long, pixels: Vector[Vector[Boolean]]) {

    override def toString: String = {
      pixels
        .map(line => line.map(bool => if (bool) '#' else '.').mkString)
        .mkString("\n")
    }

    def countTrues: Int = pixels.map(row => row.count(identity)).sum

    def borders: List[List[Boolean]] = {
      val north = pixels.head.toList
      val east = pixels.map(_.last).toList
      val south = pixels.last.reverse.toList
      val west = pixels.map(_.head).reverse.toList
      List(north, east, south, west)
    }

    def withoutBorders: Image = {
      copy(pixels =
        pixels
          .drop(1)
          .dropRight(1)
          .map(_.drop(1).dropRight(1))
      )
    }

    def transform(transformation: Transformation): Image = {
      transformation match {
        case NoTransformation => this
        case Rotate90         => rotate
        case Rotate180        => rotate.rotate
        case Rotate270        => rotate.rotate.rotate
        case Flip             => flipVertically
        case Flip90           => flipVertically.rotate
        case Flip180          => flipVertically.rotate.rotate
        case Flip270          => flipVertically.rotate.rotate.rotate
      }
    }

    def trans(f: (Int, Int) => Boolean): Image = {
      val buffer = Array.ofDim[Boolean](pixels.length, pixels.length)
      for { row <- pixels.indices; col <- pixels.indices } {
        buffer(col)(row) = f(row, col)
      }
      copy(pixels = buffer.toVector.map(_.toVector))
    }

    def rotate: Image =
      trans((row, col) => pixels(pixels.length - 1 - row)(col))

    def flipVertically: Image =
      trans((row, col) => pixels(col)(pixels.length - 1 - row))

  }

  val NORTH = 0
  val EAST = 1
  val SOUTH = 2
  val WEST = 3

  sealed abstract class Transformation(val value: Int)

  case object NoTransformation extends Transformation(0)

  case object Rotate90 extends Transformation(1)

  case object Rotate180 extends Transformation(2)

  case object Rotate270 extends Transformation(3)

  case object Flip extends Transformation(4)

  case object Flip90 extends Transformation(5)

  case object Flip180 extends Transformation(6)

  case object Flip270 extends Transformation(7)

  val allTransformations: Seq[Transformation] = List(
    NoTransformation,
    Rotate90,
    Rotate180,
    Rotate270,
    Flip,
    Flip90,
    Flip180,
    Flip270
  )

  case class ImagePlacement(image: Image, transformation: Transformation) {

    import scala.math.Ordering.Implicits.*

    override def toString = s"${image.id.toString}/$transformation"

    def >(other: ImagePlacement) = {
      (
        image.id,
        transformation.value
      ) > (other.image.id, other.transformation.value)
    }

    def <(other: ImagePlacement) = {
      (
        image.id,
        transformation.value
      ) < (other.image.id, other.transformation.value)
    }

    lazy val transformedImage: Image = image.transform(transformation)

    lazy val borders: List[List[Boolean]] = {
      transformedImage.borders
    }
  }

  def part1: Unit = {
    val input = parseInput
    val images = input.map(image => image.id -> image).toMap
    val board =
      Grid[Option[ImagePlacement]](Vector.fill(12)(Vector.fill(12)(None)))
    val result = solve(images, board, Point(0, 0))

    result.foreach(grid =>
      println(
        grid
          .cell(Point(0, 0))
          .get
          .image
          .id * grid.cell(Point(11, 0)).get.image.id * grid
          .cell(Point(0, 11))
          .get
          .image
          .id * grid.cell(Point(11, 11)).get.image.id
      )
    )

//    println(result)
  }

  def parseInput = {
    val lines = readLines()

    def parseNextImage: Option[Image] = {
      val imageLines = lines.takeWhile(_.nonEmpty).toList
      imageLines.headOption.map { header =>
        val imageId = header.drop(5).dropRight(1).toLong
        val pixels = imageLines.drop(1).map(_.map(_ == '#').toVector).toVector
        Image(imageId, pixels)
      }
    }

    @tailrec
    def parseImages(acc: Vector[Image]): Vector[Image] = {
      parseNextImage match {
        case None      => acc
        case Some(img) => parseImages(acc :+ img)
      }
    }

    parseImages(Vector.empty)
  }

  def allPlacements(image: Image): Seq[ImagePlacement] = {
    allTransformations.map(ImagePlacement(image, _))
  }

  def pickNextFittingImage(
      images: Map[Long, Image],
      board: Grid[Option[ImagePlacement]],
      pos: Point,
      currentPlacement: Option[ImagePlacement]
  ): Option[ImagePlacement] = {
    val placedImageIds = board.cells.flatten.flatten.map(_.image.id).toSet
    val borderRestrictions = List(
      board.cellOpt(pos.north()).flatten.map(_.borders(SOUTH).reverse),
      board.cellOpt(pos.east()).flatten.map(_.borders(WEST).reverse),
      board.cellOpt(pos.south()).flatten.map(_.borders(NORTH).reverse),
      board.cellOpt(pos.west()).flatten.map(_.borders(EAST).reverse)
    )
    //    println(s"Picking next image, border restrictions $borderRestrictions")
    //    println(s"placedImageIds = ${placedImageIds}")

    images.values
      .flatMap(allPlacements)
      .filter(imagePlacement => currentPlacement.forall(imagePlacement > _))
      .filterNot(placement => placedImageIds(placement.image.id))
      .filter {
        _.borders.zip(borderRestrictions).forall { case (border, restrictionOpt) =>
          restrictionOpt.forall(_ == border)
        }
      }
      .toList
      .sortBy(_.image.id)
      .headOption
  }

  def nextPosition(
      board: Grid[Option[ImagePlacement]],
      pos: Point
  ): Option[Point] = {
    val goRight = pos.east()
    if (board.withinBounds(goRight)) {
      Some(goRight)
    } else {
      val goDownNextRow = pos.copy(x = 0, y = pos.y + 1)
      Some(goDownNextRow).filter(board.withinBounds)
    }
  }

  def previousPosition(
      board: Grid[Option[ImagePlacement]],
      pos: Point
  ): Option[Point] = {
    val goLeft = pos.west()
    if (board.withinBounds(goLeft)) {
      Some(goLeft)
    } else {
      val goUpNextRow = pos.copy(x = board.width - 1, y = pos.y - 1)
      Some(goUpNextRow).filter(board.withinBounds)
    }
  }

  def solve(
      images: Map[Long, Image],
      board: Grid[Option[ImagePlacement]],
      pos: Point
  ): Option[Grid[Option[ImagePlacement]]] = {
    val nextPlacement: Option[ImagePlacement] = board.cell(pos) match {
      case None =>
        //        println(s"In solve $pos, currently no image placed yet.")
        pickNextFittingImage(images, board, pos, None)
      case Some(placement) =>
        //        println(
        //          s"In solve $pos, currently image is placed, rotating or finding new image."
        //        )
        pickNextFittingImage(
          images,
          board.updated(pos, None),
          pos,
          Some(placement)
        )
    }
    nextPlacement match {
      // backtrack
      case None =>
//        println(
//          s"In solve $pos, no new image found, backtracking to previous position."
//        )
        previousPosition(board, pos) match {
          case None          => None
          case Some(prevPos) => solve(images, board.updated(pos, None), prevPos)
        }
      case Some(placement) =>
//        println(
//          s"In solve $pos, new image/rotation found ${placement}, placing it and moving to next position."
//        )
        val updatedBoard = board.updated(pos, Some(placement))
        nextPosition(board, pos) match {
          case None         => Some(updatedBoard)
          case Some(newPos) => solve(images, updatedBoard, newPos)
        }
    }
  }

  def part2 = {
    val input = parseInput
    val images = input.map(image => image.id -> image).toMap
    val board =
      Grid[Option[ImagePlacement]](
        Vector.fill(Math.sqrt(images.size).toInt)(
          Vector.fill(Math.sqrt(images.size).toInt)(None)
        )
      )
    val result = solve(images, board, Point(0, 0)).get

    result.mapCells { (pos, placement) =>
//      println(s"At pos ${pos} found image: $placement")
//      println(placement.get.transformedImage)
    }

    val combinedImagesWithoutBorders = result
      .mapCells((_, placement) => placement.get.transformedImage.withoutBorders)
      .cells

    val actualImage = Image(
      id = -1,
      pixels = combinedImagesWithoutBorders.reverse // reverse row order!
        .foldLeft(Vector.empty[Vector[Boolean]]) { (rows, elem) =>
          val result = elem.foldLeft(Vector.empty[Vector[Boolean]]) { (cols, elem2) =>
            if (cols.isEmpty) { elem2.pixels }
            else {
              cols.zip(elem2.pixels).map { case (a, b) => a ++ b }
            }
          }
          rows ++ result
        }
    )

    val seaMonster: Vector[Vector[Boolean]] = """                  # 
                                                |#    ##    ##    ###
                                                | #  #  #  #  #  #   """.stripMargin
      .split("\n")
      .map(_.map(_ == '#').toVector)
      .toVector

    val seaMonsters =
      allPlacements(actualImage).map(placement => placement -> findSeaMonsters(placement.transformedImage, seaMonster))

    val seaMonsterTrues = seaMonster.map(_.count(identity)).sum

    seaMonsters.foreach { case (placement, monstersFound) =>
//      println(s"Image: $placement")
//      println(placement.transformedImage.toString)
//      println(s"Monsters found $monstersFound")
      println(
        s"roughness= ${placement.transformedImage.countTrues - (monstersFound * seaMonsterTrues)}"
      )
    }

  }

  def subset(
      pixels: Vector[Vector[Boolean]],
      pos: Point,
      width: Int,
      height: Int
  ): Option[Vector[Vector[Boolean]]] = {
    if (
      pos.y >= 0 && pos.y < pixels.size && pos.y + height >= 0 && pos.y + height < pixels.size &&
      pos.x >= 0 && pos.x < pixels(
        pos.y
      ).size && pos.x + width >= 0 && pos.x + width < pixels(pos.y).size
    ) {
      Some(
        pixels
          .slice(pos.y, pos.y + height)
          .map(_.slice(pos.x, pos.x + width))
      )
    } else {
      None
    }
  }

  def containsMonster(
      subset: Vector[Vector[Boolean]],
      monster: Vector[Vector[Boolean]]
  ) = {
    val matches = for {
      y <- monster.indices
      x <- monster(y).indices
    } yield !monster(y)(x) || monster(y)(x) && subset(y)(x)

    matches.forall(identity)
  }

  def findSeaMonsters(image: Image, monster: Vector[Vector[Boolean]]): Int = {
    val monsterWidth = monster.head.size
    val monsterHeight = monster.size
    val pixels = image.pixels
    val monsters: Seq[Boolean] = for {
      y <- pixels.indices
      x <- pixels(y).indices
    } yield {
      subset(pixels, Point(x, y), monsterWidth, monsterHeight).exists { sub =>
        val result = containsMonster(sub, monster)
//        if (result) println(s"Found monster at $x, $y")
        result
      }
    }
    monsters.count(identity)
  }

  part1 // 23386616781851
  part2 // 2376
}
