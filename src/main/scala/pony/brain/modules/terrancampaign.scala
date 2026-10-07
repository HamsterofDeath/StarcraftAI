package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.JavaConverters._

case class TerranCampaignConfig(minFighters: Int = 12, armyMinerals: Int = 1500,
                                armyGas: Int = 300, expansionReserve: Int = 300) {
  require(minFighters > 0 && armyMinerals >= 0 && armyGas >= 0 && expansionReserve >= 0)
  def launch(count: Int, minerals: Int, gas: Int) =
    count >= minFighters && minerals >= armyMinerals && gas >= armyGas
  def expand(unlockedMinerals: Int, unlockedGas: Int, costMinerals: Int, costGas: Int,
             pending: Boolean, safeReachableSite: Boolean) =
    !pending && safeReachableSite && unlockedMinerals >= costMinerals + expansionReserve && unlockedGas >= costGas
}

object TerranCampaignConfig {
  def load() = {
    def number(key: String, default: Int) = sys.props.get("twailight." + key).map(_.toInt).getOrElse(default)
    TerranCampaignConfig(number("minFighters", 12), number("armyMinerals", 1500),
      number("armyGas", 300), number("expansionReserve", 300))
  }
}

case class ObservedEnemyBuilding(id: Int, tile: MapTilePosition, width: Int, height: Int, base: Boolean) {
  def footprint = for (x <- tile.x until tile.x + width; y <- tile.y until tile.y + height)
    yield MapTilePosition(x, y)
}

/** Persistent knowledge consists only of native visible observations, never hidden enemy truth. */
class EnemyCampaignMemory {
  private val remembered = mutable.Map.empty[Int, ObservedEnemyBuilding]
  private var selected = Option.empty[MapTilePosition]
  def buildings = remembered.values.toVector.sortBy(_.id)
  def target = selected
  def attackPosition = selected.flatMap { site =>
    buildings.filter(_.tile.distanceToIsLess(site, 12))
      .sortBy(b => (!b.base, b.tile.distanceSquaredTo(site), b.tile.x, b.tile.y, b.id)).headOption.map(_.tile)
  }
  def update(visible: Seq[ObservedEnemyBuilding], observedDestroyed: Set[Int],
             visibleTile: MapTilePosition => Boolean): Unit = {
    visible.foreach(b => remembered.put(b.id, b))
    val visibleIds = visible.map(_.id).toSet
    remembered.retain { (id, building) =>
      !observedDestroyed(id) && (visibleIds(id) || !building.footprint.forall(visibleTile))
    }
    selected = selected.filter(t => buildings.exists(_.tile.distanceToIsLess(t, 12)))
  }
  def select(from: MapTilePosition): Option[MapTilePosition] = {
    if (selected.isEmpty) {
      selected = buildings.sortBy(b => (!b.base, b.tile.distanceSquaredTo(from), b.tile.x, b.tile.y, b.id))
        .headOption.map(_.tile)
    }
    selected
  }
}

private[pony] object ScoutPointPairs {
  def next(points: Vector[MapTilePosition], covered: Set[MapTilePosition])
          (pathLength: (MapTilePosition, MapTilePosition) => Option[Double]): List[MapTilePosition] = {
    val uncovered = points.filterNot(covered)
    if (uncovered.size <= 1) uncovered.toList
    else uncovered.combinations(2).flatMap { pair =>
      pathLength(pair(0), pair(1)).map(length => pair.toList -> length)
    }.toVector.sortBy(_._2).headOption.map(_._1).getOrElse(Nil)
  }
}

/** One campaign owns the persistent offensive target; existing arbitration and micro own commands. */
class RunTerranCampaign(universe: Universe) extends OrderlessAIModule[Mobile](universe) {
  private val config = TerranCampaignConfig.load()
  private val memory = new EnemyCampaignMemory
  private var previousTarget = Option.empty[MapTilePosition]
  private var launched = false
  override def onNth = 31

  override def onTick_!(): Unit = {
    if (!race.isTerran) return
    val visible = nativeGame.getAllUnits.asScala.iterator.filter { u =>
      u.getPlayer.isEnemy(nativeGame.self()) && u.isVisible && u.getType.isBuilding
    }.map { u =>
      val p = u.getTilePosition
      ObservedEnemyBuilding(u.getID, MapTilePosition(p.getX, p.getY), u.getType.tileWidth,
        u.getType.tileHeight, u.getType.isResourceDepot)
    }.toVector
    val before = memory.buildings.map(_.id).toSet
    memory.update(visible, world.observedDestroyedEnemies, p => nativeGame.isVisible(p.asTilePosition))
    val learned = memory.buildings.map(_.id).toSet -- before
    if (learned.nonEmpty) NativeMatchEvidence.trace("discovered-buildings", learned.toVector.sorted.mkString(","))
    val home = bases.mainBase.map(_.mainBuilding.tilePosition).getOrElse(MapTilePosition(0, 0))
    memory.select(home)
    val target = memory.attackPosition
    if (target != previousTarget) {
      worldDominationPlan.setCampaignTarget(target)
      NativeMatchEvidence.trace("campaign-target", target.toString)
      previousTarget = target
      launched = false
    }
    val fighters = ownUnits.allMobilesWithWeapons.filter { m =>
      m.isInGame && !m.isBeingCreated && m.isFigher && !m.isInstanceOf[WorkerUnit] &&
        !m.isInstanceOf[SupportUnit] && !m.isInstanceOf[TransporterUnit]
    }.groupBy(_.nativeUnitId).values.map(_.head).toVector
    val minerals = fighters.map(_.nativeUnitType.mineralPrice).sum
    val gas = fighters.map(_.nativeUnitType.gasPrice).sum
    if (!worldDominationPlan.planningInProgress && worldDominationPlan.campaignForceSize == 0) launched = false
    target.foreach { where =>
      if (!worldDominationPlan.planningInProgress &&
        (launched || config.launch(fighters.size, minerals, gas))) {
        val accepted = worldDominationPlan.initiateCampaignAttack(where)
        if (accepted) {
          NativeMatchEvidence.trace(if (launched) "reinforce" else "launch",
            s"$where count=${fighters.size} minerals=$minerals gas=$gas")
          launched = true
        }
      }
    }
  }
}
