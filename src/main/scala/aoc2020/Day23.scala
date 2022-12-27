package aoc2020

import util.AocApp

import scala.reflect.ClassTag

object Day23 extends AocApp {

  override val logOnDebug: Boolean = false

  class ListNode[A](
      var value: A,
      var previous: ListNode[A],
      var next: ListNode[A]
  ) {
    def setNext(nexts: ListNode[A]): Unit = this.next = nexts
    def setPrev(prev: ListNode[A]): Unit = this.previous = prev
  }

  def part1 = {
    val input: Vector[Int] =
      //      "364289715".map(_.asDigit).toVector
      "389125467".map(_.asDigit).toVector

    val head = buildLinkedList(input)
    val arrayByVal = buildNodesByLabel(head, input.size + 1)

    val endResult = doCrabMoves2(
      1,
      head,
      arrayByVal,
      terminateAfterRounds = 100,
      lowest = 1,
      highest = 9
    )

    val nodeOne = arrayByVal(1)
    println(collectNodes(nodeOne.next, 8).mkString)
  }

  def part2 = {

    val input: Vector[Int] = (("364289715"
      .map(_.asDigit)
      .toVector) ++ (10 to 1000000).toVector)

    val head = buildLinkedList(input)
    val arrayByVal = buildNodesByLabel(head, input.size + 1)

    val endResult = doCrabMoves2(
      1,
      head,
      arrayByVal,
      terminateAfterRounds = 10000000,
      lowest = 1,
      highest = 1000000
    )
    val one = arrayByVal(1)
    println(one.next.value.toLong * one.next.next.value.toLong)
  }

  def buildLinkedList(seq: Seq[Int]): ListNode[Int] = {
    val firstNode = new ListNode(seq.head, previous = null, next = null)
    val lastNode = seq.tail.foldLeft(firstNode) { (prev, elem) =>
      val nextNode: ListNode[Int] =
        new ListNode(elem, previous = prev, next = null)
      prev.setNext(nextNode)
      nextNode
    }
    lastNode.setNext(firstNode)
    firstNode.setPrev(lastNode)
    firstNode
  }

  def buildNodesByLabel(
      firstNode: ListNode[Int],
      size: Int
  ): Array[ListNode[Int]] = {
    val array = new Array[ListNode[Int]](size)
    var node = firstNode
    for (i <- 0 until size) {
      array(node.value) = node
      node = node.next
    }
    array
  }

  def collectNodes[A: ClassTag](head: ListNode[A], n: Int): Vector[A] = {
    val array = new Array[A](n)
    var node = head
    for (i <- 0 until n) {
      array(i) = node.value
      node = node.next
    }
    array.toVector
  }

  def doCrabMoves2(
      round: Int = 1,
      currentNode: ListNode[Int],
      nodesByLabel: Array[ListNode[Int]],
      terminateAfterRounds: Int,
      lowest: Int,
      highest: Int
  ): ListNode[Int] = {
//    if (round % 1000 == 0) println(s"-- move $round --")
//    println(
//      s"${collectNodes(currentNode, 9).mkString(" ")} ,  ( ${currentNode.value} )"
//    )
    val nextNode = crabMove2(currentNode, nodesByLabel, lowest, highest)
    if (round == terminateAfterRounds) {
//      println(
//        s"Terminating with ${collectNodes(currentNode, 9)
//          .mkString(" ")} , nextNode = : ${nextNode.value}"
//      )
      nextNode
    } else {
      doCrabMoves2(
        round + 1,
        nextNode,
        nodesByLabel,
        terminateAfterRounds,
        lowest,
        highest
      )
    }
  }

  def crabMove2(
      currentNode: ListNode[Int],
      nodesByLabel: Array[ListNode[Int]],
      lowest: Int,
      highest: Int
  ): ListNode[Int] = {
    val currentLabel = currentNode.value
//    println(s"currentLabel ${currentLabel}")

    val x = currentNode.next
    val y = x.next
    val z = y.next
    val pickupList = List(x.value, y.value, z.value)
//    println(s"picked up: $pickupList")
    val destinationLabel = findDestinationLabel(
      List(x.value, y.value, z.value),
      currentLabel - 1,
      lowest,
      highest
    )
    val destinationNode = nodesByLabel(destinationLabel)
//    println(
//      s"Found destination node ${destinationNode.value} with prev ${destinationNode.previous.value} and next ${destinationNode.next.value}"
//    )

    x.previous.setNext(z.next)
    z.next.setPrev(x.previous)
    x.setPrev(destinationNode)
    z.setNext(destinationNode.next)
    destinationNode.next.setPrev(z)
    destinationNode.setNext(x)

    currentNode.next
  }

  def findDestinationLabel(
      pickedUp: Seq[Int],
      label: Int,
      lowest: Int,
      highest: Int
  ): Int = {
//    println(
//      s"finding index for label $label}"
//    )
    if (label < lowest) {
      findDestinationLabel(pickedUp, highest, lowest, highest)
    } else if (pickedUp.contains(label)) {
      findDestinationLabel(pickedUp, label - 1, lowest, highest)
    } else {
      label
    }
  }

  part1 // 98645732
  part2 // 689500518476
}
