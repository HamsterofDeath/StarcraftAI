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

  private val BotArea     = 1
  private val TurretArea  = 2
  private val StagingArea = 3

  /**
    * A unit type of a micro scenario: its UNIT id, its name, the three-letter code that map names use, its race and its
    * production cost.
    */
  final case class Side(unitId: Int, word: String, code: String, race: Int, minerals: Int, gas: Int) {

    /** Gas is scarcer than minerals; 1.5 is the usual exchange rate when comparing armies. */
    def value: Double = minerals + 1.5 * gas
  }

  val Marine  = Side(MapUnit.Marine, "marine", "mar", UmsScenario.Terran, 50, 0)
  val Firebat = Side(MapUnit.Firebat, "firebat", "bat", UmsScenario.Terran, 50, 25)
  val Vulture = Side(MapUnit.Vulture, "vulture", "vul", UmsScenario.Terran, 75, 0)
  val Goliath = Side(MapUnit.Goliath, "goliath", "gol", UmsScenario.Terran, 100, 50)
  val Tank    = Side(MapUnit.SiegeTank, "tank", "tnk", UmsScenario.Terran, 150, 100)
  val Wraith  = Side(MapUnit.Wraith, "wraith", "wra", UmsScenario.Terran, 150, 100)
  val Zealot  = Side(MapUnit.Zealot, "zealot", "zea", UmsScenario.Protoss, 100, 0)
  val Dragoon = Side(MapUnit.Dragoon, "dragoon", "drg", UmsScenario.Protoss, 125, 50)
  val Scv     = Side(MapUnit.Scv, "scv", "scv", UmsScenario.Terran, 50, 0)
  val Medic   = Side(MapUnit.Medic, "medic", "med", UmsScenario.Terran, 50, 25)
  val Cruiser = Side(MapUnit.Battlecruiser, "battlecruiser", "bcr", UmsScenario.Terran, 400, 300)
  val Scout   = Side(MapUnit.Scout, "scout", "sco", UmsScenario.Protoss, 275, 125)
  val Archon  = Side(MapUnit.Archon, "archon", "arc", UmsScenario.Protoss, 100, 300)

  /** Resources each side fields in a pure-versus-pure matchup. */
  val MatchupBudget = 1200

  def unitsFor(side: Side, budget: Int): Int = math.max(1, math.round(budget / side.value).toInt)

  /**
    * `n` bot units against `m` computer units on an empty 64x64 map, 40 tiles apart. The computer attacks; the bot must
    * kill everything within `timeoutSeconds` of game time.
    */
  def micro(ours: Side, n: Int, theirs: Side, m: Int, timeoutSeconds: Int = 240): UmsScenario = UmsScenario(
    name = s"e2e ${fileName(ours, n, theirs, m)}",
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

  /**
    * Mixed armies: each group of the bot's and the computer's in its own block of columns, 40 tiles apart. The
    * computer attacks; the bot must kill every enemy within `timeoutSeconds` of game time.
    */
  def mixed(
      ours: Seq[(Side, Int)],
      theirs: Seq[(Side, Int)],
      brawl: Boolean = false,
      timeoutSeconds: Int = 300
  ): UmsScenario = {
    def blocks(groups: Seq[(Side, Int)], owner: Int, x: Int) = groups.zipWithIndex.flatMap { case ((side, count), i) =>
      block(side.unitId, owner, count, if (owner == 0) x - 3 * i else x + 3 * i)
    }
    // a brawl: both armies' units alternate on one grid in the middle of the map, a tile and a half apart
    def mingled = {
      def each(groups: Seq[(Side, Int)], owner: Int) =
        groups.flatMap((side, count) => Seq.fill(count)(side.unitId -> owner))
      val a       = each(ours, 0)
      val b       = each(theirs, 1)
      val all     = a.zipAll(b, (-1, -1), (-1, -1)).flatMap((x, y) => Seq(x, y)).filter(_._1 >= 0)
      val columns = 8
      all.zipWithIndex.map { case ((unitId, owner), i) =>
        MapUnit(unitId, owner, (28 + i % columns) * 48 + 16, (28 + i / columns) * 48 + 16)
      }
    }
    UmsScenario(
      name = s"e2e ${mixedFileName(ours, theirs, brawl)}",
      description = s"Bot (player 1) must kill the mixed army within $timeoutSeconds seconds.",
      widthTiles = 64,
      heightTiles = 64,
      botRace = ours.head._1.race,
      opponentRace = theirs.head._1.race,
      units = Seq(MapUnit.atTile(MapUnit.StartLocation, 0, 4, 32), MapUnit.atTile(MapUnit.StartLocation, 1, 60, 32)) ++
        (if (brawl) mingled else blocks(ours, 0, 14) ++ blocks(theirs, 1, 50)),
      locations = Seq(Location.aroundTile(BotArea, "Bot area", 12, 32, 4)),
      triggers = opponentAttacks ++ botResult(timeoutSeconds),
      groundTiles = BadlandsDirt
    )
  }

  def mixedFileName(ours: Seq[(Side, Int)], theirs: Seq[(Side, Int)], brawl: Boolean = false): String = {
    val name = (if (brawl) "x" else "") + (ours ++ theirs).map((s, n) => s"${s.code}$n").mkString("-")
    require(name.length <= MaxFileNameLength, s"Map file name '$name' is longer than $MaxFileNameLength characters")
    name
  }

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
    val name = s"${ours.code}$n-${theirs.code}$m"
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
    * cannon within `timeoutSeconds` of game time, and loses when its `ours` units are dead. `support` units come
    * along: medics attack-move with the group and heal, SCVs wait at a staging point outside the cannons' reach and
    * repair with the 1000 minerals and gas the bot gets.
    */
  def cannons(
      ours: Side,
      n: Int,
      cannons: Int,
      support: Option[(Side, Int)] = None,
      timeoutSeconds: Int = 240
  ): UmsScenario = UmsScenario(
    name = s"e2e ${cannonFileName(ours, n, cannons, support)}",
    description = s"Bot (player 1) must destroy all photon cannons within $timeoutSeconds seconds.",
    widthTiles = 64,
    heightTiles = 64,
    botRace = ours.race,
    opponentRace = UmsScenario.Protoss,
    units = Seq(MapUnit.atTile(MapUnit.StartLocation, 0, 8, 32), MapUnit.atTile(MapUnit.StartLocation, 1, 56, 32)) ++
      block(ours.unitId, 0, n, 12) ++ support.toSeq.flatMap((s, k) => block(s.unitId, 0, k, 6)) ++
      cannonColumn(cannons),
    locations = Seq(
      Location.aroundTile(BotArea, "Bot area", 12, 32, 4),
      Location.aroundTile(TurretArea, "Cannons", TurretColumn, 32, 2),
      Location.aroundTile(StagingArea, "Staging", TurretColumn - 12, 32, 2)
    ),
    triggers = Seq(
      // "Turn ON Shared Vision for Player 1", run by the opponent
      Trigger(Seq(1), Seq(Trigger.always), Seq(Trigger.runAiScript("+Vi0"))),
      Trigger(
        Seq(0),
        Seq(Trigger.always),
        Seq(
          Trigger.setResources(0, 1000),
          Trigger.order(0, Trigger.Men, Trigger.Anywhere, TurretArea, 2),
          // workers would attack-move into the cannons; they repair from behind
          Trigger.order(0, MapUnit.Scv, Trigger.Anywhere, StagingArea, 0)
        )
      ),
      Trigger(Seq(0), Seq(Trigger.commandsAtMost(1, MapUnit.PhotonCannon, 0)), Seq(Trigger.victory)),
      Trigger(Seq(0), Seq(Trigger.commandsAtMost(0, ours.unitId, 0)), Seq(Trigger.defeat)),
      Trigger(Seq(0), Seq(Trigger.elapsedSeconds(timeoutSeconds)), Seq(Trigger.defeat))
    ),
    groundTiles = BadlandsDirt
  )

  /** The photon cannon's code in map names. */
  val CannonCode = "can"

  /** The tile column the cannons' centres sit on; their pylon stands right behind them. */
  private val TurretColumn = 48

  /** 2x2 buildings have their centre on a tile corner; the cannons stand edge to edge, the pylon powers them all. */
  private def cannonColumn(count: Int) = {
    require(count >= 1 && count <= 4, "one pylon powers up to four stacked cannons")
    MapUnit(MapUnit.Pylon, 1, (TurretColumn + 2) * 32, 32 * 32) +: (0 until count).map { i =>
      MapUnit(MapUnit.PhotonCannon, 1, TurretColumn * 32, 32 * 32 + (2 * i - (count - 1)) * 32)
    }
  }

  def cannonFileName(ours: Side, n: Int, cannons: Int, support: Option[(Side, Int)] = None): String = {
    val name = s"${ours.code}$n${support.fold("")((s, k) => s"-${s.code}$k")}-$CannonCode$cannons"
    require(name.length <= MaxFileNameLength, s"Map file name '$name' is longer than $MaxFileNameLength characters")
    name
  }

  /** Ranged Terran ground and air units, about 600 resources' worth, against one and two cannons. */
  val TerranVsCannons: Map[String, UmsScenario] =
    (for {
      (ours, n) <- Seq(Marine -> 8, Vulture -> 6, Goliath -> 4, Tank -> 3, Wraith -> 4)
      count     <- Seq(1, 2)
    } yield cannonFileName(ours, n, count) -> cannons(ours, n, count)).toMap ++
      // the same groups against two cannons with two repairing SCVs or healing medics
      Seq(Marine -> 8, Vulture -> 6, Goliath -> 4, Tank -> 3, Wraith -> 4).map { (ours, n) =>
        val support = Some((if (ours == Marine) Medic else Scv) -> 2)
        cannonFileName(ours, n, 2, support) -> cannons(ours, n, 2, support)
      }

  /**
    * Mixed armies to compare focus-fire modes: Terran infantry and mech against Gateway armies and shield-heavy
    * Archons, Battlecruisers against ground (Dragoons), air (Scouts) and both; charging and brawling.
    */
  val FocusFire: Map[String, UmsScenario] = Seq(
    Seq(Marine -> 12, Medic -> 3) -> Seq(Zealot -> 4, Dragoon -> 4),
    Seq(Goliath -> 5, Tank -> 2)  -> Seq(Zealot -> 4, Dragoon -> 4),
    Seq(Marine -> 12, Medic -> 3) -> Seq(Archon -> 2, Zealot -> 2),
    Seq(Cruiser -> 3)             -> Seq(Dragoon -> 6),
    Seq(Cruiser -> 3)             -> Seq(Scout -> 4),
    Seq(Cruiser -> 3)             -> Seq(Dragoon -> 4, Scout -> 2)
  ).flatMap { (ours, theirs) =>
    // each matchup twice: the armies charge into each other, and they start mingled in one brawl
    Seq(
      mixedFileName(ours, theirs)               -> mixed(ours, theirs),
      mixedFileName(ours, theirs, brawl = true) -> mixed(ours, theirs, brawl = true)
    )
  }.toMap

  val All: Map[String, UmsScenario] = VultureScaling ++ TerranVsProtoss ++ TerranVsCannons ++ FocusFire

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
