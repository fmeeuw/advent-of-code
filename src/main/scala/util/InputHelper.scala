package util

import util.InputOps.source

trait InputHelper {

  def year: Int
  def day: Int

  def readString(suffix: Option[String] = None): String = InputOps.readString(year, day)

  def readLines(suffix: Option[String] = None): Iterator[String] = InputOps.readLines(year, day)

}
