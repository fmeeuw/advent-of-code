package util

trait AocApp extends App with InputHelper {

  def logOnDebug: Boolean

  def year: Int = {
    getClass.getPackageName.drop(3).toInt
  }
  def day: Int = {
    getClass.getSimpleName.drop(3).dropRight(1).toInt
  }

  def debug(line: Any): Unit = {
    if (logOnDebug) println(line)
  }
  def info(line: Any): Unit = {
    println(line)
  }

}
