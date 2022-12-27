package aoc2022

import util.AocApp

object Day13 extends AocApp {

  override val logOnDebug: Boolean = false
  sealed trait Data {
    def toString: String
  }
  case class Lis(values: List[Data]) extends Data {
    override def toString = s"[${values.mkString(",")}]"
  }
  case class In(value: Int) extends Data {
    override def toString = s"$value"
  }

  case class Pair(left: Data, right: Data)

  def part1 = {
    val pairs: List[Pair] = parsePairs
    val pairsWithIndices: List[(Pair, Int)] = pairs.zipWithIndex.map { case (pair, index) => (pair, index + 1) }
    pairsWithIndices.foreach { case (pair, index) =>
      debug(s"Pair $index:")
      debug(s"\tCompare:  ${pair.left} vs ${pair.right}")
      debug(s"\tPair in right order: ${compareRec(pair.left, pair.right)}")
    }
    val sumOfIndices = pairsWithIndices.collect {
      case (pair, index) if compareRec(pair.left, pair.right).contains(true) => index
    }.sum
    info(sumOfIndices)
  }

  def part2 = {
    val input = readLines().toList.filterNot(_.isBlank).map(line => parseListOrElementString(line.toList))
    val dividerPacket1: Data = parseListOrElementString("[[2]]".toList)
    val dividerPacket2 = parseListOrElementString("[[6]]".toList)
    val lines = dividerPacket1 :: dividerPacket2 :: input
    val sortedLines = lines.sortWith((left, right) => compareRec(left, right).getOrElse(true))

    info(sortedLines.zipWithIndex.collect {
      case (data, idx) if data == dividerPacket1 || data == dividerPacket2 => idx + 1
    }.product)
  }

  def compareRec(left: Data, right: Data): Option[Boolean] = {
    left -> right match
      case (In(l), In(r)) =>
        val result = if (l < r) Some(true) else if (l > r) Some(false) else None
        debug(s"Comparing ${l} with $r , result = $result")
        result
      case (Lis(l), Lis(r)) =>
        val result = compareListRec(l, r)
        debug(s"Comparing $l with $r, result = $result")
        result
      case (Lis(l), In(r)) =>
        val result = compareListRec(l, List(In(r)))
        debug(s"Comparing $l with $r, result = $result")
        result
      case (In(l), Lis(r)) =>
        val result = compareListRec(List(In(l)), r)
        debug(s"Comparing $l with $r, result = $result")
        result
  }

  def compareListRec(left: List[Data], right: List[Data]): Option[Boolean] = {
    left -> right match
      case (Nil, Nil)         => None
      case (_, Nil)           => Some(false)
      case (Nil, _)           => Some(true)
      case (l :: ls, r :: rs) => compareRec(l, r).orElse(compareListRec(ls, rs))
  }

  def parsePairs: List[Pair] = {
    val lines = readLines().toList
    lines.filterNot(_.isBlank).grouped(2).toList.map { case List(left, right) =>
      Pair(parseListOrElementString(left.toList), parseListOrElementString(right.toList))
    }
  }

  def parseListOrElementString(line: List[Char]): Data = {
    if (line.isEmpty) {
      Lis(List.empty)
    } else if (line.startsWith("[")) {
      val elements: List[Data] = parseListStringElements(line.drop(1)).map(parseListOrElementString)
      Lis(elements)
    } else {
      In(Integer.parseInt(line.mkString))
    }
  }

  def parseListStringElements(
      line: List[Char]
  ): List[List[Char]] = {

    def parseListElementsRec(
        line: List[Char],
        depth: Int,
        currentCollectedChars: List[Char],
        collectedElements: List[List[Char]]
    ): List[List[Char]] = line match
      case ::(head, next) =>
        head match
          case ']' if (depth == 0) =>
            if (currentCollectedChars.isEmpty) collectedElements else currentCollectedChars :: collectedElements
          case ',' if (depth == 0) =>
            parseListElementsRec(next, depth, List.empty, currentCollectedChars :: collectedElements)
          case '['   => parseListElementsRec(next, depth + 1, head :: currentCollectedChars, collectedElements)
          case ']'   => parseListElementsRec(next, depth - 1, head :: currentCollectedChars, collectedElements)
          case ','   => parseListElementsRec(next, depth, head :: currentCollectedChars, collectedElements)
          case other => parseListElementsRec(next, depth, head :: currentCollectedChars, collectedElements)
      case Nil => List.empty

    parseListElementsRec(line, 0, List.empty, List.empty).map(_.reverse).reverse
  }

  part1 // 5503
  part2 // 20952
}
