package util

import java.io.FileWriter
import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Try, Using}

object PuzzleInputOps {

  def readString(year: Int, day: Int, suffix: Option[String] = None): String =
    source(year, day, suffix).mkString

  def readLines(year: Int, day: Int, suffix: Option[String] = None): Iterator[String] =
    source(year, day, suffix).getLines()

  private def source(
      year: Int,
      day: Int,
      suffix: Option[String] = None
  ): BufferedSource = {
    val suffixString = suffix.map(s => s"-$s").getOrElse("")
    Source.fromFile(s"src/main/resources/aoc$year/day$day$suffixString.txt")
  }

}
