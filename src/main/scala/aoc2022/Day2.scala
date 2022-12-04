package aoc2022

import util.{AocApp, InputOps}

object Day2 extends AocApp {

  enum Shape:
    case Rock, Paper, Scissors
  enum Outcome:
    case Lose, Draw, Win

  case class Round1(opponent: Shape, player: Shape)
  case class Round2(opponent: Shape, player: Outcome)

  def part1 = parseInput1.map(round => score(round.player, round.opponent)).sum

  def part2 = parseInput2.map { round =>
    val playerShape = determineShape(round.opponent, round.player).get
    score(playerShape, round.opponent)
  }.sum

  def parseInput1: List[Round1] = {
    readLines().collect {
      case line if !line.isBlank =>
        val oppponentAction = line.charAt(0) match
          case 'A' => Shape.Rock
          case 'B' => Shape.Paper
          case 'C' => Shape.Scissors
        val playerAction = line.charAt(2) match
          case 'X' => Shape.Rock
          case 'Y' => Shape.Paper
          case 'Z' => Shape.Scissors
        Round1(oppponentAction, playerAction)
    }.toList
  }

  def parseInput2: List[Round2] = {
    readLines().collect {
      case line if !line.isBlank =>
        val oppponentAction = line.charAt(0) match
          case 'A' => Shape.Rock
          case 'B' => Shape.Paper
          case 'C' => Shape.Scissors
        val playerOutcome = line.charAt(2) match
          case 'X' => Outcome.Lose
          case 'Y' => Outcome.Draw
          case 'Z' => Outcome.Win
        Round2(oppponentAction, playerOutcome)
    }.toList
  }

  def determineShape(opponentShape: Shape, expectedOutcome: Outcome): Option[Shape] = {
    Shape.values.map(playerShape => playerShape -> outcome(playerShape, opponentShape)).collectFirst {
      case (playerShape, outcome) if outcome == expectedOutcome => playerShape
    }
  }

  def score(playerShape: Shape, opponentShape: Shape): Int = {
    val shapeScore = playerShape match
      case Shape.Rock     => 1
      case Shape.Paper    => 2
      case Shape.Scissors => 3

    val roundOutcome = outcome(playerShape, opponentShape) match
      case Outcome.Lose => 0
      case Outcome.Draw => 3
      case Outcome.Win  => 6

    shapeScore + roundOutcome
  }

  def outcome(playerShape: Shape, opponentShape: Shape): Outcome = {
    playerShape -> opponentShape match
      case (Shape.Rock, Shape.Rock)         => Outcome.Draw
      case (Shape.Rock, Shape.Paper)        => Outcome.Lose
      case (Shape.Rock, Shape.Scissors)     => Outcome.Win
      case (Shape.Paper, Shape.Rock)        => Outcome.Win
      case (Shape.Paper, Shape.Paper)       => Outcome.Draw
      case (Shape.Paper, Shape.Scissors)    => Outcome.Lose
      case (Shape.Scissors, Shape.Rock)     => Outcome.Lose
      case (Shape.Scissors, Shape.Paper)    => Outcome.Win
      case (Shape.Scissors, Shape.Scissors) => Outcome.Draw
  }

  println(part1) // 9177
  println(part2) // 12111
}
