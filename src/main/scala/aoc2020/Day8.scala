package aoc2020

import aoc2020.Day8.Operation.{Accumulate, Jump, NoOperation}
import util.AocApp

object Day8 extends AocApp {

  sealed trait Operation
  object Operation {
    def parse(value: String): Operation = {
      val List(instruction, argumentString) = value.split(' ').toList
      val argument = argumentString.toInt
      instruction match {
        case "acc" => Accumulate(argument)
        case "jmp" => Jump(argument)
        case "nop" => NoOperation(argument)
      }
    }

    case class Accumulate(argument: Int) extends Operation
    case class Jump(argument: Int) extends Operation
    case class NoOperation(argument: Int) extends Operation
  }

  case class ProgramState(index: Int, accumulator: Int)

  sealed trait ExecutionError extends TerminationResult
  case class IllegalJump(fromIndex: Int, toIndex: Int) extends ExecutionError

  sealed trait TerminationResult
  object TerminationResult {

    case object EndOfProgram extends TerminationResult

    case object LoopDetected extends TerminationResult

  }
  case class RunResult(
      endState: ProgramState,
      terminationResult: TerminationResult
  )

  def part1 = println(runProgramRec(parseInput, ProgramState(0, 0)).endState.accumulator)

  def part2 = {
    val operations = parseInput
    for {
      indexToChange <- 0 until operations.size
    } yield {
      val operationToChange = operations(indexToChange)
      val updatedOperation = operationToChange match {
        case Jump(value)        => NoOperation(value)
        case NoOperation(value) => Jump(value)
        case other              => other
      }
      val updatedOperations =
        operations.updated(indexToChange, updatedOperation)
      val result =
        runProgramRec(updatedOperations, ProgramState(0, 0))

      if (result.terminationResult != TerminationResult.LoopDetected) {
        println(result.endState.accumulator)
      }
    }
  }

  def executeOperation(
      operations: Vector[Operation],
      current: ProgramState
  ): Either[ExecutionError, ProgramState] = {
    val operation = operations(current.index)
    operation match {
      case Accumulate(value) =>
        Right(
          current.copy(index = current.index + 1, current.accumulator + value)
        )
      case Jump(jumpValue) =>
        val jumpToIndex = current.index + jumpValue
        if (jumpToIndex == current.index || jumpToIndex >= operations.size)
          Left(IllegalJump(current.index, jumpToIndex))
        else
          executeOperation(
            operations,
            current.copy(index = current.index + jumpValue)
          )
      case NoOperation(_) =>
        Right(current.copy(index = current.index + 1))
    }
  }

  def runProgramRec(
      operations: Vector[Operation],
      state: ProgramState,
      round: Int = 0,
      executedIndexes: Set[Int] = Set.empty
  ): RunResult = {
//    println(s"Running round ${round}, state = ${state}.")
    if (state.index >= operations.size) {
      RunResult(state, TerminationResult.EndOfProgram)
    } else if (executedIndexes.contains(state.index)) {
      RunResult(state, TerminationResult.LoopDetected)
    } else {
      executeOperation(operations, state) match {
        case Left(error) => RunResult(state, error)
        case Right(nextState) =>
          runProgramRec(
            operations,
            nextState,
            round + 1,
            executedIndexes + state.index
          )
      }
    }
  }

  def parseInput: Vector[Operation] =
    readLines().map(Operation.parse).toVector

  part1 // 1521
  part2 // 1016

}
