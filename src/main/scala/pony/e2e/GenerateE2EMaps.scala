package pony.e2e

import pony.e2e.chk.{ChkFile, Location, MapUnit, Trigger, UmsScenario}
import pony.e2e.mpq.{MpqArchive, MpqWriter}

import java.nio.file.{Files, Path, Paths}

/**
  * Writes the end-to-end scenario maps: `sbt "runMain pony.e2e.GenerateE2EMaps <template.scx> <outDir>"`. The template
  * must be a Badlands map such as AIIDE's (2)Destination.scx; it provides the verification code and the defaults.
  */
object GenerateE2EMaps {

  /** Plain Badlands dirt, the most common ground of (2)Destination.scx. */
  val BadlandsDirt = Seq(0x0021, 0x0031, 0x0023, 0x0033, 0x0034, 0x0024, 0x0020, 0x0030)

  private val BotArea = 1

  /** A unit type of a micro scenario: its UNIT id, its file-name word and its race. */
  final case class Side(unitId: Int, word: String, race: Int)

  val Vulture = Side(MapUnit.Vulture, "vulture", UmsScenario.Terran)
  val Zealot  = Side(MapUnit.Zealot, "zealot", UmsScenario.Protoss)

  /**
    * `n` bot units against `m` computer units on an empty 64x64 map, 40 tiles apart. The computer attacks; the bot must
    * kill everything within `timeoutSeconds` of game time.
    */
  def micro(ours: Side, n: Int, theirs: Side, m: Int, timeoutSeconds: Int = 240): UmsScenario = UmsScenario(
    name = s"e2e $n ${ours.word} vs $m ${theirs.word}",
    description = s"Bot (player 1) must kill all ${theirs.word}s within $timeoutSeconds seconds.",
    widthTiles = 64,
    heightTiles = 64,
    botRace = ours.race,
    opponentRace = theirs.race,
    units = Seq(MapUnit.atTile(MapUnit.StartLocation, 0, 8, 32), MapUnit.atTile(MapUnit.StartLocation, 1, 56, 32)) ++
      block(ours.unitId, 0, n, 12) ++ block(theirs.unitId, 1, m, 52),
    locations = Seq(Location.aroundTile(BotArea, "Bot area", 12, 32, 4)),
    triggers = opponentAttacks ++ botResult(timeoutSeconds),
    groundTiles = BadlandsDirt
  )

  /** Columns of up to six units, a tile and a half apart, centred on row 32 around column `x`. */
  private def block(unitId: Int, owner: Int, count: Int, x: Int) = (0 until count).map { i =>
    val column = i / 6
    val row    = i % 6
    val rows   = math.min(6, count - column * 6)
    MapUnit(
      unitId,
      owner,
      x * 32 + 16 + (if (owner == 0) -column else column) * 48,
      32 * 32 + 16 + (row * 48) - (rows - 1) * 24
    )
  }

  val All: Map[String, UmsScenario] = Seq((1, 1), (2, 3), (4, 6), (8, 12)).map { case (n, m) =>
    s"micro-${Vulture.word}-$n-vs-${Zealot.word}-$m" -> micro(Vulture, n, Zealot, m)
  }.toMap

  private def opponentAttacks = Seq(
    Trigger(
      Seq(1),
      Seq(Trigger.always),
      Seq(
        Trigger.runAiScript("Suic"),
        Trigger.order(1, Trigger.AnyUnit, Trigger.Anywhere, BotArea, orderType = 2)
      )
    )
  )

  private def botResult(timeoutSeconds: Int) = Seq(
    Trigger(Seq(0), Seq(Trigger.commandsAtMost(1, Trigger.Men, 0)), Seq(Trigger.victory)),
    Trigger(Seq(0), Seq(Trigger.commandsAtMost(0, Trigger.Men, 0)), Seq(Trigger.defeat)),
    Trigger(Seq(0), Seq(Trigger.elapsedSeconds(timeoutSeconds)), Seq(Trigger.defeat))
  )

  def write(scenario: UmsScenario, template: ChkFile, target: Path): Unit = {
    Files.createDirectories(target.getParent)
    Files.write(target, MpqWriter.write(Seq(InspectMap.ScenarioPath -> scenario.build(template).bytes)))
  }

  def main(args: Array[String]): Unit = {
    val template = ChkFile.parse(MpqArchive.open(Paths.get(args(0))).read(InspectMap.ScenarioPath))
    All.foreach { case (fileName, scenario) =>
      val target = Paths.get(args(1)).resolve(fileName + ".scm")
      write(scenario, template, target)
      println(s"wrote $target (${Files.size(target)} bytes)")
    }
  }
}
