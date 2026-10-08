package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class TerranCampaignConfig(minFighters: Int = 12, armyMinerals: Int = 1500,
                                armyGas: Int = 300, expansionReserve: Int = 0,
                                bankMinerals: Int = 1000, bankGas: Int = 300,
                                requiredFields: Int = 2, minScoutFighters: Int = 6,
                                minScouts: Int = 1, fieldUsefulFraction: Double = 0.15) {
  require(minFighters > 0 && armyMinerals >= 0 && armyGas >= 0 && expansionReserve >= 0 &&
    bankMinerals >= 0 && bankGas >= 0 && requiredFields >= 1 && minScoutFighters >= 1 &&
    minScouts >= 1 && fieldUsefulFraction > 0.0 && fieldUsefulFraction < 1.0)
  def launch(count: Int, minerals: Int, gas: Int) =
    count >= minFighters && minerals >= armyMinerals && gas >= armyGas
  def ready(secondBaseOperational: Boolean, count: Int, minerals: Int, gas: Int,
            bankM: Int, bankG: Int) = secondBaseOperational && launch(count, minerals, gas) &&
    bankM >= bankMinerals && bankG >= bankGas
  def holdNewArmy(secondBaseOperational: Boolean, count: Int, minerals: Int, gas: Int,
                  attackLaunched: Boolean) =
    secondBaseOperational && !attackLaunched && launch(count, minerals, gas)
  def expand(unlockedMinerals: Int, unlockedGas: Int, costMinerals: Int, costGas: Int,
             pending: Boolean, safeReachableSite: Boolean) =
    !pending && safeReachableSite && unlockedMinerals >= costMinerals + expansionReserve && unlockedGas >= costGas

  /** A mineral field at or below the configured fraction remaining is no longer worth holding. */
  def fieldUseful(remainingFraction: Double) = remainingFraction > fieldUsefulFraction
}

object TerranCampaignConfig {
  def load() = {
    def number(key: String, default: Int) = sys.props.get("twailight." + key).map(_.toInt).getOrElse(default)
    def fraction(key: String, default: Double) = sys.props.get("twailight." + key).map(_.toDouble).getOrElse(default)
    TerranCampaignConfig(number("minFighters", 12), number("armyMinerals", 1500),
      number("armyGas", 300), number("expansionReserve", 0),
      number("bankMinerals", 1000), number("bankGas", 300),
      number("requiredFields", 2), number("minScoutFighters", 6),
      number("minScouts", 1), fraction("fieldUsefulFraction", 0.15))
  }
}

case class MiningFieldStatus(id: Int, capacity: Int, assigned: Int, working: Int,
                            landedCompleted: Boolean) {
  def saturated = landedCompleted && capacity > 0 && assigned >= capacity
  def operational = landedCompleted && capacity > 0 && working > 0
}

/** Saturation is a milestone: lending a miner to construction must not cancel a funded expansion. */
private[pony] class TerranEconomicProgress {
  private var startingField = Option.empty[Int]
  private var saturated = false
  private var secondEstablished = false
  def observe(start: Option[Int], fields: Seq[MiningFieldStatus]): Unit = {
    if (startingField.isEmpty) startingField = start
    saturated ||= startingField.exists(id => fields.exists(f => f.id == id && f.saturated))
    secondEstablished ||= saturated && startingField.exists(id =>
      fields.exists(f => f.id != id && f.operational))
  }
  def startingFieldSaturated = saturated
  def secondBaseEstablished = secondEstablished
}

private[pony] object MineralFieldStaffing {
  def permitted(defaultCampaign: Boolean, landedField: Option[Int], miningField: Int) =
    !defaultCampaign || landedField.contains(miningField)
}

private[pony] case class DefenseField(id: Int, rally: MapTilePosition)
private[pony] case class DefenseFighter(id: Int, tile: MapTilePosition, campaignAssigned: Boolean = false,
                                      garrisonReserved: Boolean = false)
/** Stable guards stay with each distinct landed field; casualties are replaced locally. */
private[pony] class TerranDefenseRoster(perField: Int) {
  private var members = Map.empty[Int, Vector[Int]]
  private var fields = Map.empty[Int, MapTilePosition]
  def update(currentFields: Seq[DefenseField], fighters: Seq[DefenseFighter]): Unit = {
    fields = currentFields.map(f => f.id -> f.rally).toMap
    val mobile = fighters.filterNot(_.garrisonReserved)
    val live = mobile.map(_.id).toSet
    members = members.filter(p => fields.contains(p._1)).map { case (field, ids) => field -> ids.filter(live) }
    var used = members.values.flatten.toSet
    currentFields.sortBy(_.id).foreach { field =>
      val kept = members.getOrElse(field.id, Vector.empty)
      val replacements = mobile.filterNot(f => used(f.id) || f.campaignAssigned)
        .sortBy(f => (f.tile.distanceSquaredTo(field.rally), f.id)).take(perField - kept.size).map(_.id)
      members += field.id -> (kept ++ replacements)
      used ++= replacements
    }
  }
  def reserved = members.values.flatten.toSet
  def rallyFor(id: Int) = members.find(_._2.contains(id)).flatMap(p => fields.get(p._1))
}

/** A raid invalidates in-flight offensive plans and survives a busy planner until admitted. */
private[pony] class CampaignDefenseControl {
  var pressure = false
  var generation = 0
  var target = Option.empty[MapTilePosition]
  private var pending = Option.empty[MapTilePosition]
  def setPressure(now: Boolean): Unit = {
    if (now != pressure) generation += 1
    pressure = now
    if (!now) { target = None; pending = None }
  }
  def queue(where: MapTilePosition): Unit = { target = Some(where); pending = Some(where) }
  def takeReady(planning: Boolean): Option[MapTilePosition] = {
    if (planning) None else { val result = pending; pending = None; result }
  }
  def acceptsCampaign(version: Int) = !pressure && generation == version
}

/** A funded factory job and its visible unfinished SCV describe the same production slot. */
private[pony] object WorkerProductionQuota {
  def missing(target: Int, completed: Int, incomplete: Int, reservedTraining: Int,
              nativeTraining: Int, requests: Seq[Int]): Int = {
    val production = incomplete max (reservedTraining max nativeTraining)
    (target - completed - production - requests.sum) max 0
  }
}

/** Cargo carried from another field cannot prove that this patch is being worked. */
private[pony] object LocalMineralMining {
  def observed(mining: Boolean, assignedPatch: Int, nativeTarget: Option[Int], nearby: Boolean) =
    mining && nativeTarget.contains(assignedPatch) && nearby
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
  private var wasReady = false
  private val defenseRoster = new TerranDefenseRoster(6)
  private val defenses = oncePerTick {
    if (strategy.current.isInstanceOf[Strategy.SimpleTerran]) {
      val fields = bases.allBases.filter(b => b.mainBuilding.isInGame &&
        !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating).flatMap { base =>
        base.resourceArea.map { area =>
          val rally = strategicMap.defenseLineOf(base).flatMap(_.pointsInside
            .filter(mapLayers.freeWalkableTiles.free).toVector.sortBy(_.distanceSquaredTo(base.mainBuilding.centerTile)).headOption)
            .getOrElse(area.nearbyFreeTile)
          DefenseField(area.uniqueId, rally)
        }
      }.groupBy(_.id).values.map(_.head).toVector
      defenseRoster.update(fields, fighters.map(m => DefenseFighter(m.nativeUnitId, m.currentTile,
        worldDominationPlan.attackOf(m).exists(_.campaign),
        universe.pluginByType[TerranBunkerDefense].reserved(m))))
    } else defenseRoster.update(Nil, Nil)
    defenseRoster
  }
  def isReservedDefender(unit: WrapsUnit) = defenses.get.reserved(unit.nativeUnitId) ||
    universe.pluginByType[TerranBunkerDefense].reserved(unit)
  def guardPosition(unit: WrapsUnit) = defenses.get.rallyFor(unit.nativeUnitId)
  override def onNth = 31

  private def fighters = ownUnits.allMobilesWithWeapons.filter { m =>
    m.isInGame && !m.isBeingCreated && m.isFigher && !m.isInstanceOf[WorkerUnit] &&
      !m.isInstanceOf[SupportUnit] && !m.isInstanceOf[TransporterUnit]
  }.groupBy(_.nativeUnitId).values.map(_.head).toVector
  private def expedition = fighters.filterNot(isReservedDefender).filterNot(m =>
    worldDominationPlan.attackOf(m).exists(a => !a.campaign))

  def reconnaissanceAllowed = {
    val troops = expedition
    val funds = resources.currentResources
    val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
    operational && !worldDominationPlan.baseDefenseActive && (launched ||
      (universe.pluginByType[TerranBunkerDefense].defenseSufficient && config.ready(operational,
        troops.size, troops.map(_.nativeUnitType.mineralPrice).sum, troops.map(_.nativeUnitType.gasPrice).sum,
        funds.minerals, funds.gas)))
  }

  def enemyLocated = memory.buildings.nonEmpty

  /** One minimal scout may go out early purely to locate the enemy for the campaign memory. */
  def minimalScoutingActive = {
    val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
    !enemyLocated && !worldDominationPlan.baseDefenseActive &&
      (operational || fighters.size >= config.minScoutFighters)
  }

  def scoutingAllowed = reconnaissanceAllowed || minimalScoutingActive

  // Keep spending toward the launch reserve instead of freezing a rich bank.
  def holdingNewArmy = {
    val troops = expedition
    val funds = resources.currentResources
    !worldDominationPlan.baseDefenseActive && config.holdNewArmy(universe.pluginByType[ManageMiningAtBases].secondBaseEstablished,
      troops.size, troops.map(_.nativeUnitType.mineralPrice).sum,
      troops.map(_.nativeUnitType.gasPrice).sum, launched) &&
      funds.minerals < config.bankMinerals + 600 &&
      funds.gas < config.bankGas + 300
  }

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
    val troops = expedition
    val minerals = troops.map(_.nativeUnitType.mineralPrice).sum
    val gas = troops.map(_.nativeUnitType.gasPrice).sum
    if (!worldDominationPlan.planningInProgress && worldDominationPlan.campaignForceSize == 0) launched = false
    val ready = reconnaissanceAllowed
    if (ready && !wasReady) NativeMatchEvidence.trace("offense-ready",
      s"establishedSecondBase=true fighters=${troops.size} army=$minerals/$gas bank=${resources.currentResources}")
    wasReady = ready
    target.foreach { where =>
      if (ready && !worldDominationPlan.planningInProgress) {
        val accepted = worldDominationPlan.initiateCampaignAttack(where, troops.map(_.nativeUnitId).toSet)
        if (accepted) {
          NativeMatchEvidence.trace(if (launched) "reinforce" else "launch",
            s"$where count=${troops.size} minerals=$minerals gas=$gas")
          launched = true
        }
      }
    }
  }
}
