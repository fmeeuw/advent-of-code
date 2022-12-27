package aoc2022

import util.AocApp

object Day7 extends AocApp {

  override val logOnDebug: Boolean = false

  sealed trait Command
  case class ChangeDirectoryCommand(path: String) extends Command
  case class ListCommand(output: List[ListOutput]) extends Command

  sealed trait ListOutput
  case class FileOutput(name: String, size: Long) extends ListOutput
  case class DirectoryOutput(name: String) extends ListOutput

  def part1 = {
    val input = readLines().toList
    val commands = parseCommands(input)
    val (last, directSizes) = commands.foldLeft("" -> Map.empty[String, Long]) { case ((current, map), command) =>
      command match
        case ChangeDirectoryCommand(path) if path == "/" => path -> map
        case ChangeDirectoryCommand(path) if path == ".." =>
          (current.reverse.drop(1).dropWhile(_ != '/').reverse) -> map
        case ChangeDirectoryCommand(path) => (current + s"$path/") -> map
        case ListCommand(output) => {
          val sum = output.collect { case FileOutput(_, size) =>
            size
          }.sum
          current -> map.updated(current, sum)
        }
    }

    val allDirs = directSizes.keySet
    val recursiveSizes = allDirs.map(dir => dir -> allDirs.filter(_.startsWith(dir)).map(directSizes.apply).sum).toMap

    info(recursiveSizes.filter { case (_, size) => size <= 100000 }.values.sum)
  }

  def part2 = {

    val input = readLines().toList
    val commands = parseCommands(input)
    val (last, directSizes) = commands.foldLeft("" -> Map.empty[String, Long]) { case ((current, map), command) =>
      command match
        case ChangeDirectoryCommand(path) if path == "/" => path -> map
        case ChangeDirectoryCommand(path) if path == ".." =>
          (current.reverse.drop(1).dropWhile(_ != '/').reverse) -> map
        case ChangeDirectoryCommand(path) => (current + s"$path/") -> map
        case ListCommand(output) => {
          val sum = output.collect { case FileOutput(_, size) =>
            size
          }.sum
          current -> map.updated(current, sum)
        }
    }

    val allDirs = directSizes.keySet
    val recursiveSizes = allDirs.map(dir => dir -> allDirs.filter(_.startsWith(dir)).map(directSizes.apply).sum).toMap

    val totalSpace = 70000000
    val neededSpace = 30000000
    val usedSpace = recursiveSizes("/")
    val minimumToDelete = usedSpace - (totalSpace - neededSpace)

    // closest minimum
    val closestMin = recursiveSizes.values.foldLeft(Long.MaxValue)((min, size) =>
      if (size > minimumToDelete && size < min) size else min
    )

    info(closestMin)
  }

  private def parseCommands(inputLines: List[String]): List[Command] = {
    parseChangeDirectoryCommand(inputLines).orElse(parseListCommand(inputLines)) match
      case Some((command, remaining)) => command :: parseCommands(remaining)
      case None                       => Nil
  }

  private def parseChangeDirectoryCommand(inputLines: List[String]): Option[(ChangeDirectoryCommand, List[String])] = {
    inputLines match
      case ::(line, next) if line.startsWith("$ cd") => Some(ChangeDirectoryCommand(line.drop(5)) -> next)
      case _                                         => None
  }

  private def parseListCommand(inputLines: List[String]): Option[(ListCommand, List[String])] = {
    def parseListOutput(line: String): ListOutput = {
      if (line.startsWith("dir")) DirectoryOutput(line.drop(4))
      else {
        line.split("\\s").toList match
          case List(size, name) => FileOutput(name, size.toLong)
      }
    }

    inputLines match
      case ::(line, next) if line.startsWith("$ ls") =>
        Some(
          ListCommand(next.takeWhile(line => !line.startsWith("$")).map(parseListOutput)) -> next.dropWhile(line =>
            !line.startsWith("$")
          )
        )
      case _ => None
  }

  part1
  part2

}
