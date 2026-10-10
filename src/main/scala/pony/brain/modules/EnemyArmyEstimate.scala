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
