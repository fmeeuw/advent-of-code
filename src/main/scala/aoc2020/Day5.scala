package aoc2020

import util.AocApp

import scala.collection.BitSet
import scala.io.Source

object Day5 extends AocApp {

  override val logOnDebug: Boolean = false

  case class Ticket(rowNr: Int, colNr: Int) {
    lazy val seatId: Int = rowNr * 8 + colNr
  }

  def part1: Unit = {
    println(parseInput.map(_.seatId).max)
  }

  def part2: Unit = {
    val occupiedTickets = parseInput.toSet
    val openTickets = for {
      row <- 1 until 128
      col <- 1 until 8
      ticket = Ticket(row, col)
      if !occupiedTickets.contains(ticket)
    } yield {
      ticket
    }

    val occupiedSeatIds = occupiedTickets.map(_.seatId)
    val mySeat = openTickets.filter { ticket =>
      occupiedSeatIds.contains(ticket.seatId - 1) &&
      occupiedSeatIds.contains(ticket.seatId + 1)
    }

    println(mySeat.head.seatId)
  }

  def parseInput: Vector[Ticket] = {
    val lines = readLines().toVector
    lines.map { line =>
      parseTicket(line.take(7), line.slice(7, 10))
    }
  }

  def parseTicket(rows: String, cols: String): Ticket = {
    val rowNr: Int = intValue(rows.map {
      case 'F' => '0'
      case 'B' => '1'
    })
    lazy val colNr: Int = intValue(cols.map {
      case 'L' => '0'
      case 'R' => '1'
    })
    Ticket(rowNr, colNr)
  }

  def intValue(bitString: String): Int = {
    Integer.parseInt(bitString, 2)
  }

  part1 // 850
  part2 // 599
}
