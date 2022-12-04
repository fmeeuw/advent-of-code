package aoc2020

import util.InputOps

import java.util.regex.MatchResult
import scala.annotation.tailrec

object Day19 extends App {

  case class Input(rules: Map[Int, Rule], messages: List[String])

  case class Rule(id: Int, matcher: RuleMatcher)

  sealed trait MatchResult {
    def matchesFully: Boolean
  }
  case class Match(remaining: String) extends MatchResult {
    override def matchesFully: Boolean = remaining.isEmpty
  }
  case object NoMatch extends MatchResult {
    override def matchesFully: Boolean = false
  }

  sealed trait RuleMatcher extends PartialMatcher
  sealed trait PartialMatcher {
    def matchesFull(rules: Map[Int, Rule])(line: String): Boolean = {
      matches(rules)(line) match {
        case Match(remaining) if remaining.isEmpty => true
        case _                                     => false
      }
    }

    def matches(rules: Map[Int, Rule])(line: String): MatchResult
  }

  case class MatchDisjunctions(matchRules: List[MatchRuleIds]) extends RuleMatcher {
    override def matches(rules: Map[Int, Rule])(line: String): MatchResult = {
      val result = matchRules.foldLeft[MatchResult](NoMatch)((agg, elem) =>
        agg match {
          case Match(remaining) => Match(remaining)
          case NoMatch          => elem.matches(rules)(line)
        }
      )
//      println(
//        s"matches MatchDisjunctions $matchRules for $line, Result = $result"
//      )
      result
    }
  }
  case class MatchRuleIds(ruleIds: List[MatchRuleId]) extends PartialMatcher {
    override def matches(rules: Map[Int, Rule])(line: String): MatchResult = {
      val result = ruleIds.foldLeft[MatchResult](Match(line)) { (agg, elem) =>
        agg match {
          case Match(remaining) => elem.matches(rules)(remaining)
          case NoMatch          => NoMatch
        }
      }
//      println(s"matches MatchRuleIds $ruleIds for $line is reslut $result")
      result
    }
  }
  case class ExactMatch(value: String) extends RuleMatcher {
    override def matches(rules: Map[Int, Rule])(line: String): MatchResult = {
      val result = if (line.startsWith(value)) {
        Match(line.substring(value.size))
      } else {
        NoMatch
      }
//      println(s"matches ExactMatch $value for $line is result $result")
      result
    }
  }

  case class MatchRuleId(rule: Int) extends PartialMatcher {
    override def matches(rules: Map[Int, Rule])(line: String): MatchResult = {
      val result = rules(rule).matcher.matches(rules)(line)
//      println(s"matches MatchRuleId $rule for $line is result $result")
      result
    }
  }

  def part1 = {
    val input = parseInput()
    val messagesMatchingRule0 = input.messages
      .count { line => stringMatchesRule(input.rules)(0, line) }

    println(messagesMatchingRule0)
  }

  def part2 = {
    val input = parseInput()
    val messagesMatchingRule0 = input.messages
      .count { line => matchesPart2(input.rules)(line) }

    println(messagesMatchingRule0)
  }

  /** 61: 99 21 | 36 6 6: 54 116 36: "b"
    */
  def parseInput(suffix: Option[String] = None): Input = {
    val lines = InputOps.readLines(2020, 19, suffix = suffix).toList
    val rules = lines
      .takeWhile(_.nonEmpty)
      .map { line =>
        val List(id, matcher) = line.split(": ").toList
        val matchers = if (matcher.startsWith("\"")) {
          ExactMatch(matcher.drop(1).dropRight(1))
        } else {
          val subRules = matcher.split("\\|").map(_.trim)
          MatchDisjunctions(
            subRules
              .map(subRule =>
                MatchRuleIds(
                  subRule.split("\\s").map(s => MatchRuleId(s.toInt)).toList
                )
              )
              .toList
          )
        }
        Rule(id.toInt, matchers)
      }
      .map(rule => rule.id -> rule)
      .toMap

//    lines.foreach(println)
    val messages = lines.dropWhile(_.nonEmpty).drop(1).toList
    Input(rules, messages)
  }

  def stringMatchesRule(
      rules: Map[Int, Rule]
  )(ruleId: Int, line: String): Boolean = {
    val rule = rules(ruleId)
    rule.matcher.matchesFull(rules)(line)
  }

  // 11: 42 31 | 42 11 31  =>   42 31   | 42(+) 31 31 (+)
  // 8: 42 | 42 8    =>  42 (+)
  // 0: 8 11        => 42 {m >= 2} 31 {m >= 1}
  def matchesPart2(rules: Map[Int, Rule])(line: String): Boolean = {

    val results = for {
      m <- 2 to 100
      n <- 1 to 100
      if m > n
    } yield {
      val result = matchesN(rules)(m, rules(42).matcher, line)
      result match {
        case NoMatch => NoMatch
        case Match(remaining) =>
          matchesN(rules)(n, rules(31).matcher, remaining)
      }
    }
    results.exists(_.matchesFully)
  }

  def matchesN(
      rules: Map[Int, Rule]
  )(n: Int, matcher: PartialMatcher, string: String): MatchResult = {
    if (n <= 0) {
      Match(string)
    } else {
      matcher.matches(rules)(string) match {
        case NoMatch => NoMatch
        case Match(remaining) =>
          matchesN(rules)(n - 1, matcher, remaining)
      }
    }
  }

  part1 // 213
  part2 // 325
}
