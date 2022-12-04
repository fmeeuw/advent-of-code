package util

trait AocApp extends App with InputHelper {

  def year: Int = {
    getClass.getPackageName.drop(3).toInt
  }
  def day: Int = {
    getClass.getSimpleName.drop(3).dropRight(1).toInt
  }

}
