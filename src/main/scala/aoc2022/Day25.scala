package aoc2022

import util.{AocApp, CharHelper}

object Day25 extends AocApp {

  override val logOnDebug: Boolean = false

  def part1 = {
    val snafus = parseInput
    val sum = snafus.map(snafuToLong).sum
    val snafuSum = longToSnafu(sum)
    info(snafuSum)
  }

  /** 1=-0-2 174
    *
    * @param snafu
    */
  def snafuToLong(snafu: String): Long = {
    val positives = snafu.map(char => if (char.isDigit) char else '0')
    val negatives = snafu.map(char => if (char == '=') '2' else if (char == '-') '1' else '0')
    val positiveNumber = java.lang.Long.parseLong(positives, 5).toLong
    val negativeNumber = java.lang.Long.parseLong(negatives, 5).toLong
    val result = positiveNumber - negativeNumber
    debug(
      s" Snafu number $snafu converts to number ($positives => $positiveNumber) - ${negatives} => ${negativeNumber} = $result"
    )
    result
  }

  def longToSnafu(number: Long): String = {
    val radix5String: String = java.lang.Long.toString(number, 5)
    debug(s"In longToSnafu, ${number} => in radix5 = ${radix5String}")
    val (endString, endRemember) = radix5String.foldRight((List.empty[Char], 0)) { case (char, (agg, remember)) =>
      val actualNumber = CharHelper.charToInt(char) + remember
      if (actualNumber >= 0 && actualNumber < 3) {
        (CharHelper.intToChar(actualNumber) :: agg) -> 0
      } else if (actualNumber == 3) {
        ('=' :: agg) -> 1
      } else if (actualNumber == 4) {
        ('-' :: agg) -> 1
      } else if (actualNumber == 5) {
        ('0' :: agg) -> 1
      } else {
        throw new IllegalStateException(s"wtf, actualNumber = $actualNumber")
      }
    }
    val snafu: Seq[Char] = if (endRemember == 1) { '1' :: endString }
    else endString

    debug(s"longToSnafu conversion: ${number} converted to snafu $snafu       ( $endString with $endRemember )")
    snafu.mkString
  }
  def parseInput: List[String] = readLines().toList

  part1
}
