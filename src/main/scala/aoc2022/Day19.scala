package aoc2022

import util.AocApp

import scala.util.matching.Regex

object Day19 extends AocApp {

  case class Blueprint(
      id: Int,
      oreRobotCost: Materials,
      clayRobotCost: Materials,
      obsidianRobotCost: Materials,
      geodeRobotCost: Materials
  ) {

    def highestCost(material: Material): Int = {
      Material.values.map(costForRobot(_).valueFor(material)).max
    }
    def costForRobot(material: Material): Materials = {
      material match
        case Material.Ore      => oreRobotCost
        case Material.Clay     => clayRobotCost
        case Material.Obsidian => obsidianRobotCost
        case Material.Geode    => geodeRobotCost
    }
  }

  case class Materials(ore: Int, clay: Int, obsidian: Int, geodes: Int) {

    def valueFor(material: Material): Int = {
      material match
        case Material.Ore      => ore
        case Material.Clay     => clay
        case Material.Obsidian => obsidian
        case Material.Geode    => geodes
    }
    def add(material: Material, amount: Int) = {
      material match
        case Material.Ore      => copy(ore = ore + amount)
        case Material.Clay     => copy(clay = clay + amount)
        case Material.Obsidian => copy(obsidian = obsidian + amount)
        case Material.Geode    => copy(geodes = geodes + amount)
    }

    def contains(materials: Materials) = {
      ore >= materials.ore && clay >= materials.clay && obsidian >= materials.obsidian && geodes >= materials.geodes
    }

    def +(materials: Materials) =
      copy(
        ore = ore + materials.ore,
        clay = clay + materials.clay,
        obsidian = obsidian + materials.obsidian,
        geodes = geodes + materials.geodes
      )
    def -(materials: Materials) =
      copy(
        ore = ore - materials.ore,
        clay = clay - materials.clay,
        obsidian = obsidian - materials.obsidian,
        geodes = geodes - materials.geodes
      )
  }
  object Materials {
    def Empty = Materials(0, 0, 0, 0)
  }

  enum Material:
    case Ore, Clay, Obsidian, Geode

  case class State(materials: Materials, robots: Materials) {
    def applyActionAndProduce(blueprint: Blueprint, action: BuildRobotAction): State = {
      action.material match
        case Some(value) =>
          copy(
            materials = materials - blueprint.costForRobot(value) + robots,
            robots = robots.add(value, 1)
          )
        case None =>
          copy(
            materials = materials + robots
          )
    }

  }

  case class BuildRobotAction(material: Option[Material])
  def part1 = {
    val bluePrints = parseBlueprints()
    println(bluePrints)

    // find largest number of geodes that can be opened in 24 minutes.
//    val geodesList =
//      bluePrints.map { bluePrint =>
//        val geodes = doTurnRec(24, bluePrint, State(Materials.Empty, Materials.Empty.add(Material.Ore, 1)))
//        println(s"Calculated geodes for blueprint with id ${bluePrint.id} = $geodes")
//        bluePrint.id -> geodes
//      }
//    println(geodesList)

    val geodesList2 =
      bluePrints.map { bluePrint =>
        val geodes = doTurnRec2(
          List(Candidate(24, bluePrint, State(Materials.Empty, Materials.Empty.add(Material.Ore, 1)))),
          Map.empty
        )
        println(s"Calculated geodes for blueprint with id ${bluePrint.id} = $geodes")
        bluePrint.id -> geodes
      }
    println(geodesList2)
    println(geodesList2.map { case (id, geodes) => id * geodes }.sum)
  }

  def part2 = {
    val bluePrints = parseBlueprints().take(3)
    println(bluePrints)

//    println(bluePrints.map { bluePrint =>
//      val geodes = doTurnRec(32, bluePrint, State(Materials.Empty, Materials.Empty.add(Material.Ore, 1)))
//      println(s"Calculated geodes 29 for blueprint with id ${bluePrint.id} = $geodes")
//      bluePrint.id -> geodes
//    })

    val geodesList2 =
      bluePrints.map { bluePrint =>
        val geodes = doTurnRec2(
          List(Candidate(32, bluePrint, State(Materials.Empty, Materials.Empty.add(Material.Ore, 1)))),
          Map.empty
        )
        println(s"Calculated geodes for blueprint with id ${bluePrint.id} = $geodes")
        geodes
      }
    println(geodesList2.product)
    // 32683 too low.
    // 37191
    /** round 28 round 29 1 12,3 2 7,2 3 22,6
      */

  }

  case class Candidate(remainingMinutes: Int, blueprint: Blueprint, state: State)
  def doTurnRec2(candidates: List[Candidate], maxGeodesPerRemainingMinutes: Map[Int, Int]): Int = {
    def possibleNextActions(blueprint: Blueprint, state: State): List[BuildRobotAction] = {
      if (state.materials.contains(blueprint.costForRobot(Material.Geode))) {
        List(BuildRobotAction(Some(Material.Geode)))
      } else {
        val remainingMats = List(Material.Obsidian, Material.Clay, Material.Ore).filter(mat =>
          state.materials.contains(blueprint.costForRobot(mat))
        )
        if (remainingMats.size == 3) { // can build all of them, so go ahead try all.
          remainingMats.map(mat => BuildRobotAction(Some(mat)))
        } else { // also consider doing nothing
          remainingMats.map(mat => BuildRobotAction(Some(mat))) :+ BuildRobotAction(None)
        }
      }
    }

    candidates match
      case ::(Candidate(remainingMinutes, blueprint, state), next) => {
        if (remainingMinutes == 0) {
          doTurnRec2(
            next,
            maxGeodesPerRemainingMinutes.updatedWith(remainingMinutes)(
              _.map(Math.max(_, state.materials.geodes)).orElse(Some(state.materials.geodes))
            )
          )
        } else {

          val possibleNextCandidates =
            if (
              maxGeodesPerRemainingMinutes
                .get(remainingMinutes)
                .exists(_ > (state.materials.geodes + 2)) // prune search space
            ) { // exists better state, don't traverse anymore
              List.empty
            } else {
              possibleNextActions(blueprint, state).map(action =>
                Candidate(
                  remainingMinutes - 1,
                  blueprint,
                  state.applyActionAndProduce(blueprint, action)
                )
              )
            }
          doTurnRec2(
            possibleNextCandidates ++ next,
            maxGeodesPerRemainingMinutes.updatedWith(remainingMinutes)(
              _.map(Math.max(_, state.materials.geodes)).orElse(Some(state.materials.geodes))
            )
          )
        }
      }
      case Nil => maxGeodesPerRemainingMinutes(0)
  }

  def doTurnRec(remainingMinutes: Int, blueprint: Blueprint, state: State): Int = {
    def possibleNextActions: List[BuildRobotAction] = {
      val possibleActions: List[BuildRobotAction] =
        if (state.materials.contains(blueprint.costForRobot(Material.Geode))) {
          List(BuildRobotAction(Some(Material.Geode)))
        } else {
          val remainingMats = List(Material.Obsidian, Material.Clay, Material.Ore).filter(mat =>
            state.materials.contains(blueprint.costForRobot(mat))
          )
          if (remainingMats.size == 3) { // can build all of them, so go ahead try all.
            remainingMats.map(mat => BuildRobotAction(Some(mat)))
          } else { // also consider doing nothing
            remainingMats.map(mat => BuildRobotAction(Some(mat))) :+ BuildRobotAction(None)
          }
        }

      possibleActions.collect {
        case BuildRobotAction(Some(material))
            if (material == Material.Geode || (state.robots.valueFor(material) <= blueprint.highestCost(material))) =>
          BuildRobotAction(Some(material))
        case BuildRobotAction(None) => BuildRobotAction(None)
      }
    }

    if (remainingMinutes == 0) {
      state.materials.geodes
    } else {
      val possibleNextStates: List[Int] = possibleNextActions.map(action =>
        doTurnRec(
          remainingMinutes - 1,
          blueprint,
          state.applyActionAndProduce(blueprint, action)
        )
      )
      possibleNextStates.max
    }
  }

  def parseBlueprints(suffix: Option[String] = None): List[Blueprint] = {
    def toMaterial(text: String): Material = text match
      case "ore"      => Material.Ore
      case "clay"     => Material.Clay
      case "obsidian" => Material.Obsidian

    val regex: Regex =
      """^Blueprint (\d+): Each ore robot costs (\d+) (\w+)\. Each clay robot costs (\d+) (\w+)\. Each obsidian robot costs (\d+) (\w+) and (\d+) (\w+)\. Each geode robot costs (\d+) (\w+) and (\d+) (\w+)\.$""".r
    readLines(suffix).toList.map { line =>
      val hit = regex.findFirstMatchIn(line).getOrElse(throw new IllegalStateException("Unable to parsee"))
      Blueprint(
        id = hit.group(1).toInt,
        oreRobotCost = Materials.Empty.add(toMaterial(hit.group(3)), hit.group(2).toInt),
        clayRobotCost = Materials.Empty.add(toMaterial(hit.group(5)), hit.group(4).toInt),
        obsidianRobotCost = Materials.Empty
          .add(toMaterial(hit.group(7)), hit.group(6).toInt)
          .add(toMaterial(hit.group(9)), hit.group(8).toInt),
        geodeRobotCost = Materials.Empty
          .add(toMaterial(hit.group(11)), hit.group(10).toInt)
          .add(toMaterial(hit.group(13)), hit.group(12).toInt)
      )
    }

    // Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian.

  }

  part1 // 1262
  part2 // 32683

}
