package pony.e2e

import pony.e2e.GenerateE2EMaps._
import pony.e2e.chk.{ChkFile, Location, MapUnit, Trigger, UmsScenario}
import pony.e2e.mpq.MpqArchive

import java.nio.file.{Files, Paths}
import scala.util.Random

/**
  * A battery of random battles to learn from how a fight ends: `sbt "runMain pony.e2e.GenerateBattery <template.scx>
  * <outDir> <count> <seed>"` writes b0001.scx and onwards plus manifest.json with the armies of every battle. The bot
  * fields one to three Terran unit types worth a random budget, the computer one or two Protoss types worth 0.4 to 2.5
  * times as much, so both outcomes occur. The computer charges and shares its vision: a result counts the enemy's
  * survivors even after the bot lost its last unit.
  */
object GenerateBattery {

  val Corsair = Side(MapUnit.Corsair, "corsair", "cor", UmsScenario.Protoss, 150, 100)

  val TerranPool  = Seq(Marine, Firebat, Medic, Vulture, Goliath, Tank, Wraith, Cruiser)
  val ProtossPool = Seq(Zealot, Dragoon, Archon, Scout, Corsair)

  /** The names BWAPI reports the types by, which the results count survivors by. */
  val BwapiNames: Map[Side, String] = Map(
    Marine  -> "Terran_Marine",
    Firebat -> "Terran_Firebat",
    Medic   -> "Terran_Medic",
    Vulture -> "Terran_Vulture",
    Goliath -> "Terran_Goliath",
    Tank    -> "Terran_Siege_Tank_Tank_Mode",
    Wraith  -> "Terran_Wraith",
    Cruiser -> "Terran_Battlecruiser",
    Zealot  -> "Protoss_Zealot",
    Dragoon -> "Protoss_Dragoon",
    Archon  -> "Protoss_Archon",
    Scout   -> "Protoss_Scout",
    Corsair -> "Protoss_Corsair"
  )

  val MinBudget  = 500
  val MaxBudget  = 3000
  val MinRatio   = 0.4
  val MaxRatio   = 2.5
  val MaxPerType = 24

  /** Slow killers such as wraiths against zealots need several minutes. */
  val TimeoutSeconds = 420

  /** Tile columns where the armies start: the bot's groups grow west from the first, the computer's east. */
  private val OurColumn   = 18
  private val TheirColumn = 48
  private val Rows        = 8

  final case class Battle(id: String, ours: Seq[(Side, Int)], theirs: Seq[(Side, Int)]) {
    def value(army: Seq[(Side, Int)]): Double = army.map((s, n) => s.value * n).sum
  }

  def sample(random: Random, index: Int): Battle = {
    val budget = MinBudget + random.nextDouble() * (MaxBudget - MinBudget)
    val ratio  = math.exp(math.log(MinRatio) + random.nextDouble() * (math.log(MaxRatio) - math.log(MinRatio)))
    Battle(f"b$index%04d", army(random, TerranPool, 3, budget), army(random, ProtossPool, 2, budget * ratio))
  }

  /** One to `kinds` types sharing `budget` at random weights; medics come only with infantry to heal. */
  def army(random: Random, pool: Seq[Side], kinds: Int, budget: Double): Seq[(Side, Int)] = {
    val picked  = random.shuffle(pool).take(1 + random.nextInt(kinds))
    val healing = picked.exists(s => s == Marine || s == Firebat)
    val types   = if (picked.contains(Medic) && !healing) picked.map(s => if (s == Medic) Marine else s).distinct
    else picked
    val weights = types.map(_ => 0.5 + random.nextDouble())
    types.zip(weights).map { (side, weight) =>
      side -> math.round(budget * weight / weights.sum / side.value).toInt.max(1).min(MaxPerType)
    }
  }

  def scenario(battle: Battle): UmsScenario = UmsScenario(
    name = s"e2e battle ${battle.id}",
    description = s"Bot (player 1) must kill the enemy army within $TimeoutSeconds seconds.",
    widthTiles = 64,
    heightTiles = 64,
    botRace = UmsScenario.Terran,
    opponentRace = UmsScenario.Protoss,
    units = Seq(MapUnit.atTile(MapUnit.StartLocation, 0, 4, 32), MapUnit.atTile(MapUnit.StartLocation, 1, 60, 32)) ++
      groups(battle.ours, 0, OurColumn) ++ groups(battle.theirs, 1, TheirColumn),
    locations = Seq(Location.aroundTile(BotArea, "Bot area", 12, 32, 4)),
    // "Turn ON Shared Vision for Player 1", run by the opponent
    triggers = Trigger(Seq(1), Seq(Trigger.always), Seq(Trigger.runAiScript("+Vi0"))) +:
      (opponentAttacks ++ botResult(TimeoutSeconds)),
    groundTiles = BadlandsDirt
  )

  /** Each type in its own block of columns of up to eight units, a tile and a half apart. */
  private def groups(army: Seq[(Side, Int)], owner: Int, column: Int): Seq[MapUnit] = {
    val direction = if (owner == 0) -1 else 1
    army.foldLeft((Vector.empty[MapUnit], 0.0)) { case ((placed, offset), (side, count)) =>
      val units = (0 until count).map { i =>
        val rows = math.min(Rows, count - i / Rows * Rows)
        val x    = (column + direction * (offset + i / Rows * 1.5)) * 32 + 16
        MapUnit(side.unitId, owner, x.round.toInt, 32 * 32 + 16 + i % Rows * 48 - (rows - 1) * 24)
      }
      (placed ++ units, offset + math.ceil(count.toDouble / Rows) * 1.5 + 1.5)
    }._1
  }

  def manifest(battles: Seq[Battle]): String = battles.map { b =>
    def army(groups: Seq[(Side, Int)]) = groups.map { (s, n) =>
      s"""{"code":"${s.code}","unit":"${BwapiNames(s)}","count":$n,"value":${s.value}}"""
    }.mkString("[", ",", "]")
    s"""{"id":"${b.id}","mode":"charge","ours":${army(b.ours)},"theirs":${army(b.theirs)},""" +
      s""""oursValue":${b.value(b.ours)},"theirsValue":${b.value(b.theirs)}}"""
  }.mkString("[\n", ",\n", "\n]\n")

  def main(args: Array[String]): Unit = {
    val template = ChkFile.parse(MpqArchive.open(Paths.get(args(0))).read(InspectMap.ScenarioPath))
    val out      = Paths.get(args(1))
    val seed     = args.lift(3).fold(1L)(_.toLong)
    val random   = new Random(seed)
    val battles  = (1 to args(2).toInt).map(sample(random, _))
    battles.foreach(b => write(scenario(b), template, out.resolve(b.id + ".scx")))
    Files.writeString(out.resolve("manifest.json"), manifest(battles))
    println(s"wrote ${battles.size} battles and manifest.json to $out (seed $seed)")
  }
}
