package aoc2022

import util.AocApp

import java.util
import scala.collection.mutable

object Day20 extends AocApp {

  class Node[A](val value: A, var previous: Option[Node[A]], var next: Option[Node[A]]) {
    override def toString: String = {
      value.toString
    }
    def nodesReversed(acc: List[Node[A]] = List.empty): List[Node[A]] = {
      next match
        case Some(value) => value.nodesReversed(this :: acc)
        case None        => this :: acc
    }
  }

  class LinkedList[A](var head: Node[A], var tail: Node[A]) {
//    private var byOriginalIdx: mutable.Map[Int, Node[A]] = mutable.Map(0 -> head)
    override def toString: String = {
      head.nodesReversed(List.empty).map(_.value).reverse.mkString(",")
    }

//    def getByOriginalIdx(idx: Int): Option[Node[A]] = {
//      byOriginalIdx.get(idx)
//    }

    def move(current: Node[A], steps: Int, size: Int): Unit = {
      val nodeToMoveTo = findNodeToMoveToRec(current, steps, size)
      if (steps == 0) ()
      else if (steps > 0) moveAfter(current, nodeToMoveTo)
      else moveBefore(current, nodeToMoveTo)
    }
    def findNodeToMoveToRec(current: Node[A], steps: Int, size: Int): Node[A] = {
      val reducedSteps = steps % (size - 1)
//      println(s"Iterating $current steps=$steps , current head=${head}, current tail=${tail}")
      if (reducedSteps == 0) current
      else if (reducedSteps > 0) findNodeToMoveToRec(current.next.getOrElse(head), reducedSteps - 1, size)
      else findNodeToMoveToRec(current.previous.getOrElse(tail), reducedSteps + 1, size)
    }

    def iterate(current: Node[A], i: Int, size: Int): Node[A] = {
      val index = if (i < 0) ((size - 1 + i) % (size - 1)) else (i % (size - 1))
      println(s"Iterating $current i=$i , current head=${head}, current tail=${tail}")
      if (index == 0) current
      else iterate(current.next.getOrElse(head), index - 1, size)
    }

    def iterateReal(current: Node[A], i: Int, size: Int): Node[A] = {
      val index = if (i < 0) ((size + i) % (size)) else (i % (size))
      //      println(s"Iterating $current i=$i , current head=${head}, current tail=${tail}")
      if (index == 0) current
      else iterate(current.next.getOrElse(head), index - 1, size)
    }

    def moveAfter(node: Node[A], otherNode: Node[A]): Unit = {
      if (node != otherNode) {
        remove(node)
        insertBetween(node, Some(otherNode), otherNode.next)
      }
    }

    def moveBefore(node: Node[A], otherNode: Node[A]): Unit = {
      if (node != otherNode) {
        remove(node)
        insertBetween(node, otherNode.previous, Some(otherNode))
      }
    }
    def remove(node: Node[A]): Unit = {
      node.previous.foreach(_.next = node.next)
      node.next.foreach(_.previous = node.previous)

      if (node.next.isEmpty) {
        tail = node.previous.get
      }
      if (node.previous.isEmpty) {
        head = node.next.get
      }

      node.previous = None
      node.next = None
    }

    def insertBetween(node: Node[A], after: Option[Node[A]], before: Option[Node[A]]) = {
      assert(after.nonEmpty || before.nonEmpty)

      node.previous = after
      node.next = before
      after.foreach(_.next = Some(node))
      before.foreach(_.previous = Some(node))

      if (node.next.isEmpty) {
        tail = node
      }
      if (node.previous.isEmpty) {
        head = node
      }
    }

    def append(value: A): Unit = {
      var newNode = new Node(value, previous = Some(tail), next = None)
      tail.next = Some(newNode)
      tail = newNode
//      byOriginalIdx.update(originalIdx, newNode)
    }

    def prepend(value: A): Unit = {
      var newNode = Node(value, previous = None, next = Some(head))
      head.previous = Some(newNode)
      head = newNode
//      byOriginalIdx.update(originalIdx, newNode)
    }
  }

  object LinkedList {
    def from(seq: Iterable[Int]): LinkedList[(Int, Int)] = {
      var head = new Node[(Int, Int)](seq.head -> 0, None, None)
      var list = new LinkedList[(Int, Int)](head, head)
      seq.zipWithIndex.tail.foreach { case (nr, idx) =>
        list.append(nr -> idx)
      }
      list
    }
  }
  def part1 = {

    val originalNumbers = parseNumbers()
    println(originalNumbers.mkString(","))

    val size = originalNumbers.size
    var list = LinkedList.from(originalNumbers)
    println(list)

    for { i <- 0 until size } {
      var node: Node[(Int, Int)] = list.head.nodesReversed().find(_.value._2 == i).get
//      var node = list.getByOriginalIdx(i).get
//      val node = list.tail.nodes().find(_.value == originalNumbers(i)).get
      println(s"i=$i , moving node $node")
      list.move(node, node.value._1, size)
      println(list)
    }
    println(list)
    println(list.head.nodesReversed().size)

    var zeroth = list.head.nodesReversed().find(_.value._1 == 0).get
    var oneK = list.findNodeToMoveToRec(zeroth, 1000, size + 1)
    var twoK = list.findNodeToMoveToRec(zeroth, 2000, size + 1)
    var threeK = list.findNodeToMoveToRec(zeroth, 3000, size + 1)
    println(s"Values, 1000=$oneK, 2000=$twoK, 3000=$threeK")
    println(oneK.value._1 + twoK.value._1 + threeK.value._1)

    // 4045 too high
    // 3073 too high
    // -15199 wrong
    // 90 wrong
    // 10619??
    // // -3151 wrong
    // 9610
    // 4045 wrong
    // 2827
  }

//  def part1Two = {
//    val originalNumbers = parseNumbers()
//    println(originalNumbers.mkString(","))
//
//    val size = originalNumbers.size
//    val list = scala.collection.Dou
//
//    var list = LinkedList.from(originalNumbers)
//
//  }

  def parseNumbers(suffix: Option[String] = None): List[Int] = {
    readLines(suffix).map(_.toInt).toList
  }

  part1 // -3151

}
