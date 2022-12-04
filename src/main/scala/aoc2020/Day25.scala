package aoc2020

import util.InputOps

import scala.annotation.tailrec

object Day25 extends App {

  def part1 = {
    val (cardPublicKey, doorPublicKey) = parseInput
//    val (cardPublicKey, doorPublicKey) = "5764801".toLong -> "17807724".toLong

    val cardLoopSize = detectLoopSize(cardPublicKey, 7, 3816000)
    val cardEncryptionKey = memoizedTransform(TransformKey(doorPublicKey, cardLoopSize, 20201227))
    println(s"card encryption key = $cardEncryptionKey")

    val doorLoopSize = detectLoopSize(doorPublicKey, 7)
    val doorEncryptionKey = memoizedTransform(TransformKey(cardPublicKey, doorLoopSize, 20201227))
    println(s"door encryption key = $doorEncryptionKey")
  }

  def parseInput: (Long, Long) = {
    val List(cardPublicKey, doorPublicKey) = InputOps.readLines(2020, 25).toList.map(_.toLong)
    cardPublicKey -> doorPublicKey
  }

  /** The handshake used by the card and the door involves an operation that transforms a subject number. To transform a
    * subject number, start with the value 1. Then, a number of times called the loop size, perform the following steps:
    *
    * Set the value to itself multiplied by the subject number. Set the value to the remainder after dividing the value
    * by 20201227.
    */
  case class TransformKey(subjectNr: Long, loops: Long, mod: Long)

  def memoize[K, V](f: K => V): K => V = {
    val cache = scala.collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, f(k))
  }

  val memoizedTransform: TransformKey => Long =
    memoize { (key: TransformKey) =>
      if (key.loops <= 0) 1L
      else {
        ((key.subjectNr) * (memoizedTransform(key.copy(loops = key.loops - 1)))) % key.mod
      }
    }

//  def transformSubjectNr(memory: Map[TransformKey, Long])(subjectNr: Long, loops: Long, mod: Long = 20201227): Long = {
//    memoize {
//      val key = TransformKey(subjectNr, loops, mod)
//      memory.get(key) match {
//        case Some(result) =>
////        println(s"cache hit: $key -> $result")
//          memory -> result
//        case None =>
////        println(s"cache miss: $key ")
//          if (loops <= 0) memory.updated(key, 1L) -> 1L
//          else {
//            ((transformSubjectNr(memory)(subjectNr, loops - 1) % mod) * (subjectNr % mod)) % mod
//          }
//      }
//    }
//  }

  @tailrec
  def detectLoopSize(publicKey: Long, subjectNr: Long, loopSizeToTry: Long = 1): Long = {
//    if (loopSizeToTry % 1000 == 0)
    println(s"in detectLoopSize for publicKey $publicKey and subjectNr $subjectNr, loopsize $loopSizeToTry")
    val key = TransformKey(subjectNr, loopSizeToTry, 20201227)
    val result = memoizedTransform(key)
    if (publicKey == result) {
      loopSizeToTry
    } else {
      detectLoopSize(publicKey, subjectNr, loopSizeToTry + 1)
    }
  }

  part1
}
