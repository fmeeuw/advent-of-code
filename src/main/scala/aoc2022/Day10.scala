package aoc2022

import util.AocApp

object Day10 extends AocApp {

  override val logOnDebug: Boolean = false

  sealed trait Op
  case class AddX(amount: Int) extends Op
  case object NoOp extends Op
  case class State(cycle: Long, registerHistory: List[Long])

  def part1 = {
    val initialState = State(1, List(1))
    val ops = parseInput()
    val endState = doProgram(initialState, ops)
    debug(endState)
    val signalStrenghts = for {
      cycle <- Range(20, endState.registerHistory.size, 40)
    } yield cycle * endState.registerHistory(cycle - 1)
    debug(signalStrenghts)
    info(signalStrenghts.sum)
  }

  def part2 = {
    val initialState = State(1, List(1))
    val ops = parseInput()
    val endState = doProgram(initialState, ops)
    val results: Seq[IndexedSeq[Char]] =
      (0 until 6).map { y =>
        (0 until 40).map { x =>
          val cycle = x + y * 40
          val register = endState.registerHistory(cycle)
          debug(s"cycle = ${cycle} , register = $register, X = ${x}")
          if (register >= x - 1 && register <= x + 1) '#' else '.'
        }
      }
    info(results.map(_.mkString).mkString("\n"))
  }

  private def parseInput(suffix: Option[String] = None): List[Op] =
    readLines(suffix).map(line => if (line.startsWith("addx")) AddX(line.drop(5).toInt) else NoOp).toList

  private def doProgram(initialState: State, ops: List[Op]) = {
    val stateReversed = ops.foldLeft(initialState) { case (State(cycle, history @ register :: _), op) =>
      op match
        case AddX(amount) => State(cycle + 2, (register + amount) :: register :: history)
        case NoOp         => State(cycle = cycle + 1, register :: history)
    }
    stateReversed.copy(registerHistory = stateReversed.registerHistory.reverse)
  }

  part1 // 12560
  part2 // PLPAFBCL
}
