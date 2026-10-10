package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * An upper bound on the enemy's army, and on what of it can be at a given place. Every enemy base earns what one of
  * ours does at most (measured from our own income), each main from the first frame and an expansion from shortly
  * before we first saw it; from that come the buildings we saw, the workers such an income needs and the army we
  * killed. What we saw of their army recently elsewhere cannot be at the place in question too.
  */
class EnemyArmyEstimate(universe: Universe) extends OrderlessAIModule[WrapsUnit](universe) {
  import EnemyEstimate._

  private val types      = mutable.HashMap.empty[Int, bwapi.UnitType]
  private val baseSince  = mutable.HashMap.empty[Int, Int]
  private val lastArmy   = mutable.HashMap.empty[Int, (MapTilePosition, Int, Double)]
  private var bestRate   = DefaultRatePerBase
  private var rateMark   = Option.empty[(Int, Int)]
  private var lastReport = -1

  private def value(t: bwapi.UnitType) = (t.mineralPrice + t.gasPrice).toDouble

  override def onTick_!(): Unit = {
    val now   = currentTick
    val self  = nativeGame.self()
    val start = nativeGame.getStartLocations.asScala.toVector.filterNot(_ == self.getStartLocation)
    nativeGame.getAllUnits.asScala.iterator.filter(u => u.getPlayer.isEnemy(self) && u.isVisible).foreach { u =>
      val kind = u.getType
      types(u.getID) = kind
      val tile = MapTilePosition(u.getTilePosition.getX, u.getTilePosition.getY)
      if (kind.isResourceDepot && !baseSince.contains(u.getID)) {
        val main = start.exists(s => MapTilePosition(s.getX, s.getY).distanceToIsLess(tile, 10))
        baseSince(u.getID) = if (main) 0 else math.max(EarliestExpansion, now - ExpansionLead)
      }
      if (!kind.isBuilding && !kind.isWorker && kind.canAttack) lastArmy(u.getID) = (tile, now, value(kind))
    }
    val destroyed = world.observedDestroyedEnemies
    destroyed.foreach { id => baseSince.remove(id); lastArmy.remove(id) }
    // our income per mining base, the best seen over a game minute, stands for what a base of theirs earns at most
    val gathered = self.gatheredMinerals + self.gatheredGas
    val mining   = bases.allBases.count(b => b.mainBuilding.isInGame && !b.mainBuilding.isFloating)
    rateMark match {
      case Some((frame, before)) if now - frame >= RateWindow =>
        if (mining > 0) bestRate = math.max(bestRate, (gathered - before).toDouble / (now - frame) / mining)
        rateMark = Some((now, gathered))
      case None => rateMark = Some((now, gathered))
      case _    =>
    }
    if (now / ReportFrames != lastReport) {
      lastReport = now / ReportFrames
      val e = estimate(now)
      NativeMatchEvidence.trace(
        "enemy-estimate",
        s"bases=${e.bases} rate=${"%.2f".format(bestRate)} gathered=${e.gathered.round} buildings=${e.buildings.round} " +
          s"workers=${e.workers.round} lost=${e.lostArmy.round} upper=${e.upper.round} seenArmy=${seenArmy(now).round}"
      )
    }
  }

  /** The current bound, with the main of every enemy assumed from the first frame even if not yet seen. */
  def estimate(now: Int = currentTick): Estimate = {
    val destroyed = world.observedDestroyedEnemies
    val mains     = nativeGame.enemies.size
    val seenMains = baseSince.values.count(_ == 0)
    val starts    = baseSince.values.toVector ++ Vector.fill(math.max(0, mains - seenMains))(0)
    val buildings = types.collect { case (_, t) if t.isBuilding => value(t) }.sum
    val lost      = types.collect { case (id, t) if destroyed(id) && !t.isBuilding && !t.isWorker => value(t) }.sum
    Model(now, starts, bestRate, buildings, lost).estimate
  }

  /** Army value seen within the last half minute; with `awayFrom`, only what was farther than `FarTiles` from it. */
  def seenArmy(now: Int = currentTick, awayFrom: Option[MapTilePosition] = None): Double =
    lastArmy.values.filter { (tile, at, _) =>
      now - at < RecentFrames && awayFrom.forall(_.distanceToIsMore(tile, FarTiles))
    }.map(_._3).sum

  /** The most enemy army that can stand at `tile` now: the bound less what was just seen elsewhere. */
  def maxArmyAt(tile: MapTilePosition): Double = math.max(0.0, estimate().upper - seenArmy(awayFrom = Some(tile)))
}

/** The arithmetic of the enemy army bound, kept free of the game; values in minerals plus gas, times in frames. */
private[pony] object EnemyEstimate {

  /** A saturated base earns about this much a frame (some 900 minerals and 300 gas a game minute). */
  val DefaultRatePerBase = 0.8

  /** A base reaches its full income this long after it starts (workers have to be trained first). */
  val RampFrames = 24 * 60 * 6

  /** An expansion may have run this long before we saw it, but not before the third minute. */
  val ExpansionLead      = 24 * 60 * 3
  val EarliestExpansion  = 24 * 60 * 3

  /** Workers a full income needs per base, and what they cost. */
  val WorkersPerBase = 19
  val WorkerCost     = 50

  val StartMinerals = 50
  val RateWindow    = 24 * 60
  val ReportFrames  = 24 * 60
  val RecentFrames  = 24 * 30
  val FarTiles      = 25

  final case class Estimate(bases: Int, gathered: Double, buildings: Double, workers: Double, lostArmy: Double) {
    def upper: Double = math.max(0.0, gathered + StartMinerals - buildings - workers - lostArmy)
  }

  final case class Model(now: Int, baseStarts: Seq[Int], ratePerBase: Double, buildings: Double, lostArmy: Double) {
    def estimate = Estimate(
      baseStarts.size,
      baseStarts.map(s => income(math.max(0, now - s), ratePerBase)).sum,
      buildings,
      workers,
      lostArmy
    )

    /** At least the four starting workers; a full income needs WorkersPerBase on every base. */
    def workers: Double = math.max(4, baseStarts.size * WorkersPerBase - 4).toDouble * WorkerCost
  }

  /** What a base earns in `frames`: rising from a fifth to the full rate over RampFrames, then steady. */
  def income(frames: Int, rate: Double): Double = {
    val ramp = math.min(frames, RampFrames).toDouble
    val rising = ramp * rate * (0.2 + 0.4 * ramp / RampFrames)
    rising + math.max(0, frames - RampFrames) * rate
  }
}
