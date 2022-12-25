package util

object CharHelper {

  def intToChar(number: Int): Char = {
    java.lang.Character.forDigit(number, Character.MAX_RADIX)
  }

  def charToInt(char: Char): Int = {
    char.asDigit
  }

}
