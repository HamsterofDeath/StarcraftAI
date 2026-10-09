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

  private val BotArea    = 1
  private val TurretArea = 2

  /** A unit type of a micro scenario: its UNIT id, its file-name word, its race and its production cost. */
  final case class Side(unitId: Int, word: String, race: Int, minerals: Int, gas: Int) {

    /** Gas is scarcer than minerals; 1.5 is the usual exchange rate when comparing armies. */
    def value: Double = minerals + 1.5 * gas
  }

  val Marine  = Side(MapUnit.Marine, "marine", UmsScenario.Terran, 50, 0)
  val Firebat = Side(MapUnit.Firebat, "firebat", UmsScenario.Terran, 50, 25)
  val Vulture = Side(MapUnit.Vulture, "vulture", UmsScenario.Terran, 75, 0)
  val Goliath = Side(MapUnit.Goliath, "goliath", UmsScenario.Terran, 100, 50)
  val Tank    = Side(MapUnit.SiegeTank, "tank", UmsScenario.Terran, 150, 100)
  val Wraith  = Side(MapUnit.Wraith, "wraith", UmsScenario.Terran, 150, 100)
  val Zealot  = Side(MapUnit.Zealot, "zealot", UmsScenario.Protoss, 100, 0)
  val Dragoon = Side(MapUnit.Dragoon, "dragoon", UmsScenario.Protoss, 125, 50)

  /** Resources each side fields in a pure-versus-pure matchup. */
  val MatchupBudget = 1200

  def unitsFor(side: Side, budget: Int): Int = math.max(1, math.round(budget / side.value).toInt)

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

  /**
    * StarCraft keeps 31 characters of a map file name; longer names show no map info and cannot be started. Warm
    * sessions add a four-character "NNN-" prefix, so names stay within 23 characters before the ".scx".
    */
  val MaxFileNameLength = 23

  def fileName(ours: Side, n: Int, theirs: Side, m: Int): String = {
    val name = s"${ours.word}$n-${theirs.word}$m"
    require(name.length <= MaxFileNameLength, s"Map file name '$name' is longer than $MaxFileNameLength characters")
    name
  }

  /** Vultures against Zealots at growing sizes, the first kiting benchmark. */
  val VultureScaling: Map[String, UmsScenario] = Seq((1, 1), (2, 3), (4, 6), (8, 12)).map { case (n, m) =>
    fileName(Vulture, n, Zealot, m) -> micro(Vulture, n, Zealot, m)
  }.toMap

  /** Every Terran unit against every Protoss unit, both sides worth about `MatchupBudget` resources. */
  val TerranVsProtoss: Map[String, UmsScenario] = (for {
    ours   <- Seq(Marine, Firebat, Vulture, Goliath, Tank, Wraith)
    theirs <- Seq(Zealot, Dragoon)
  } yield {
    val n = unitsFor(ours, MatchupBudget)
    val m = unitsFor(theirs, MatchupBudget)
    // slow killers such as wraiths against zealots need more than the default four minutes
    fileName(ours, n, theirs, m) -> micro(ours, n, theirs, m, timeoutSeconds = 600)
  }).toMap

  /**
    * `n` bot units attack `cannons` photon cannons, stacked in a column on one pylon 36 tiles away. The opponent
    * shares its vision, so a unit that steps out of the cannons' reach still sees them; the bot must destroy every
    * cannon within `timeoutSeconds` of game time.
    */
  def cannons(ours: Side, n: Int, cannons: Int, timeoutSeconds: Int = 240): UmsScenario = UmsScenario(
    name = s"e2e $n ${ours.word} vs $cannons cannon",
    description = s"Bot (player 1) must destroy all photon cannons within $timeoutSeconds seconds.",
    widthTiles = 64,
    heightTiles = 64,
    botRace = ours.race,
    opponentRace = UmsScenario.Protoss,
    units = Seq(MapUnit.atTile(MapUnit.StartLocation, 0, 8, 32), MapUnit.atTile(MapUnit.StartLocation, 1, 56, 32)) ++
      block(ours.unitId, 0, n, 12) ++ cannonColumn(cannons),
    locations = Seq(
      Location.aroundTile(BotArea, "Bot area", 12, 32, 4),
      Location.aroundTile(TurretArea, "Cannons", TurretColumn, 32, 2)
    ),
    triggers = Seq(
      // "Turn ON Shared Vision for Player 1", run by the opponent
      Trigger(Seq(1), Seq(Trigger.always), Seq(Trigger.runAiScript("+Vi0"))),
      Trigger(Seq(0), Seq(Trigger.always), Seq(Trigger.order(0, Trigger.Men, Trigger.Anywhere, TurretArea, 2))),
      Trigger(Seq(0), Seq(Trigger.commandsAtMost(1, MapUnit.PhotonCannon, 0)), Seq(Trigger.victory)),
      Trigger(Seq(0), Seq(Trigger.commandsAtMost(0, Trigger.Men, 0)), Seq(Trigger.defeat)),
      Trigger(Seq(0), Seq(Trigger.elapsedSeconds(timeoutSeconds)), Seq(Trigger.defeat))
    ),
    groundTiles = BadlandsDirt
  )

  /** The tile column the cannons' centres sit on; their pylon stands right behind them. */
  private val TurretColumn = 48

  /** 2x2 buildings have their centre on a tile corner; the cannons stand edge to edge, the pylon powers them all. */
  private def cannonColumn(count: Int) = {
    require(count >= 1 && count <= 4, "one pylon powers up to four stacked cannons")
    MapUnit(MapUnit.Pylon, 1, (TurretColumn + 2) * 32, 32 * 32) +: (0 until count).map { i =>
      MapUnit(MapUnit.PhotonCannon, 1, TurretColumn * 32, 32 * 32 + (2 * i - (count - 1)) * 32)
    }
  }

  def cannonFileName(ours: Side, n: Int, cannons: Int): String = {
    val name = s"${ours.word}$n-cannon$cannons"
    require(name.length <= MaxFileNameLength, s"Map file name '$name' is longer than $MaxFileNameLength characters")
    name
  }

  /** Ranged Terran ground and air units, about 600 resources' worth, against one and two cannons. */
  val TerranVsCannons: Map[String, UmsScenario] = (for {
    (ours, n) <- Seq(Marine -> 8, Vulture -> 6, Goliath -> 4, Tank -> 3, Wraith -> 4)
    count     <- Seq(1, 2)
  } yield cannonFileName(ours, n, count) -> cannons(ours, n, count)).toMap

  val All: Map[String, UmsScenario] = VultureScaling ++ TerranVsProtoss ++ TerranVsCannons

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
      val target = Paths.get(args(1)).resolve(fileName + ".scx")
      write(scenario, template, target)
      println(s"wrote $target (${Files.size(target)} bytes)")
    }
  }
}
