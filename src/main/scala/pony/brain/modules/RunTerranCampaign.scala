package pony
package brain
package modules

import scala.jdk.CollectionConverters._

/** One campaign owns the persistent offensive target; existing arbitration and micro own commands. */
class RunTerranCampaign(universe: Universe) extends OrderlessAIModule[Mobile](universe) {
  private val config = TerranCampaignConfig.load()
  private val memory = new EnemyCampaignMemory

  /** The enemy buildings seen and not known to be gone. */
  def enemyBuildings            = memory.buildings
  private var previousTarget    = Option.empty[MapTilePosition]
  private var launched          = false
  private var wasReady          = false
  private var campaignCommitted = false
  private var huntPoints        = Vector.empty[MapTilePosition]
  private var huntIndex         = 0
  private var huntTarget        = Option.empty[MapTilePosition]
  private var huntInitiated     = false
  // Bunker crews hold the mineral lines; every other fighter keeps up the pressure unless field guards are configured.
  private val defenseRoster = new TerranDefenseRoster(config.fieldGuards)
  private val defenses      = oncePerTick {
    if (strategy.current.runsTerranCampaign) {
      val fields = bases.allBases.filter(b =>
        b.mainBuilding.isInGame &&
          !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating
      ).flatMap { base =>
        base.resourceArea.map { area =>
          val rally = strategicMap.defenseLineOf(base).flatMap(_.pointsInside
            .filter(
              mapLayers.freeWalkableTiles.free
            ).toVector.sortBy(_.distanceSquaredTo(base.mainBuilding.centerTile)).headOption)
            .getOrElse(area.nearbyFreeTile)
          DefenseField(area.uniqueId, rally)
        }
      }.groupBy(_.id).values.map(_.head).toVector
      // at nearly maxed supply nothing more can be built: every field guard joins the attack
      val supplyCapped = nativeGame.self().supplyUsed >= 2 * TerranCampaignConfig.SupplyCappedAt
      defenseRoster.update(
        fields,
        if (supplyCapped) Nil
        else fighters.map(m =>
          DefenseFighter(
            m.nativeUnitId,
            m.currentTile,
            worldDominationPlan.attackOf(m).exists(_.campaign) || carpetPost(m.nativeUnitId).isDefined,
            universe.pluginByType[TerranBunkerDefense].reserved(m)
          )
        )
      )
    } else defenseRoster.update(Nil, Nil)
    defenseRoster
  }
  def fieldGuardReserved(id: Int)         = defenses.get.reserved(id)
  private def carpetPost(id: Int)         = universe.pluginByType[CarpetSpread].postOf(id)
  def isReservedDefender(unit: WrapsUnit) = defenses.get.reserved(unit.nativeUnitId) ||
    carpetPost(unit.nativeUnitId).isDefined ||
    universe.pluginByType[TerranBunkerDefense].reserved(unit)
  def guardPosition(unit: WrapsUnit) =
    carpetPost(unit.nativeUnitId).orElse(defenses.get.rallyFor(unit.nativeUnitId))
  override def onNth = 31

  // Behaviours ask these questions for every unit every frame; each answer walks the whole army, so it is computed
  // once per tick of this module (every heavy tick).
  private val fightersNow = oncePerTick {
    ownUnits.allMobilesWithWeapons.filter { m =>
      m.isInGame && !m.isBeingCreated && m.isFigher && !m.isInstanceOf[WorkerUnit] &&
      !m.isInstanceOf[SupportUnit] && !m.isInstanceOf[TransporterUnit]
    }.groupBy(_.nativeUnitId).values.map(_.head).toVector
  }
  private val reconnaissanceNow = oncePerTick(evaluateReconnaissance)
  private val minimalScoutNow   = oncePerTick(evaluateMinimalScouting)
  private def fighters          = fightersNow.get
  private def expedition        = fighters.filterNot(isReservedDefender).filterNot(m =>
    worldDominationPlan.attackOf(m).exists(a => !a.campaign)
  )

  def reconnaissanceAllowed = reconnaissanceNow.get

  private def evaluateReconnaissance = {
    val troops      = expedition
    val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
    // An army carrying the reserve's value inside it needs no further banked reserve.
    val armyValue    = troops.map(_.nativeUnitType.mineralPrice).sum
    val armyGas      = troops.map(_.nativeUnitType.gasPrice).sum
    val overwhelming = troops.size >= config.minFighters * 2 &&
      armyValue >= config.armyMinerals + config.bankMinerals && armyGas >= config.armyGas + config.bankGas
    // Always keep pressure on: attack as soon as the army is big enough, without waiting for full bunker coverage or
    // a banked reserve, and always when supply is nearly maxed, because nothing more can be built anyway.
    // ... as long as that supply holds an army: a base full of workers sends no handful of fighters to die
    val supplyCapped = nativeGame.self().supplyUsed >= 2 * TerranCampaignConfig.SupplyCappedAt &&
      troops.size >= config.minFighters
    operational && !worldDominationPlan.baseDefenseActive &&
    (launched || overwhelming || supplyCapped || config.pressure(troops.size, armyValue, armyGas))
  }

  def enemyLocated = memory.buildings.nonEmpty

  /** One minimal scout may go out early purely to locate the enemy for the campaign memory. */
  def minimalScoutingActive = minimalScoutNow.get

  private def evaluateMinimalScouting = {
    val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
    !enemyLocated && !worldDominationPlan.baseDefenseActive &&
    (operational || fighters.size >= config.minScoutFighters)
  }

  def scoutingAllowed = reconnaissanceAllowed || minimalScoutingActive

  def campaignLaunchEnabled = !strategy.current.runsTerranCampaign || strategy.current.usesCampaignLaunch

  // Keep spending toward the launch reserve instead of freezing a rich bank.
  def holdingNewArmy = campaignLaunchEnabled && {
    val troops = expedition
    val funds  = resources.currentResources
    !worldDominationPlan.baseDefenseActive && config.holdNewArmy(
      universe.pluginByType[ManageMiningAtBases].secondBaseEstablished,
      troops.size,
      troops.map(_.nativeUnitType.mineralPrice).sum,
      troops.map(_.nativeUnitType.gasPrice).sum,
      launched
    ) &&
    funds.minerals < config.bankMinerals + 600 &&
    funds.gas < config.bankGas + 300
  }

  override def onTick_!(): Unit = {
    // refreshes the per-tick answers (fighters, defenses, scouting gates)
    super.onTick_!()
    if (!race.isTerran) return
    val visible = nativeGame.getAllUnits.asScala.iterator.filter { u =>
      u.getPlayer.isEnemy(nativeGame.self()) && u.isVisible && u.getType.isBuilding
    }.map { u =>
      val p = u.getTilePosition
      ObservedEnemyBuilding(
        u.getID,
        MapTilePosition(p.getX, p.getY),
        u.getType.tileWidth,
        u.getType.tileHeight,
        u.getType.isResourceDepot
      )
    }.toVector
    val before = memory.buildings.map(_.id).toSet
    memory.update(visible, world.observedDestroyedEnemies, p => nativeGame.isVisible(p.asTilePosition))
    val learned = memory.buildings.map(_.id).toSet -- before
    if (learned.nonEmpty) NativeMatchEvidence.trace("discovered-buildings", learned.toVector.sorted.mkString(","))
    val home = bases.mainBase.map(_.mainBuilding.tilePosition).getOrElse(MapTilePosition(0, 0))
    memory.select(home)
    val known = memory.attackPosition
    // With the enemy wiped from memory but the match still running, sweep the map for remnants
    // instead of letting the army stand idle waiting for a slow scout to find the last building.
    val sweepDone = !worldDominationPlan.planningInProgress && worldDominationPlan.campaignForceSize == 0
    if (known.isDefined) {
      huntTarget = None
      huntInitiated = false
    } else if ((campaignCommitted || reconnaissanceAllowed) && !worldDominationPlan.baseDefenseActive) {
      // No enemy building seen yet: an army ready to attack must not wait for a scout. The hunt goes to the possible
      // enemy starts first (on a two-player map, the enemy main), then sweeps the fields. Not earlier: a target makes
      // the attack planning run in the background, and its pool is shared with construction planning.
      if (huntTarget.isEmpty || (huntInitiated && sweepDone)) {
        if (huntPoints.isEmpty) {
          val own         = nativeGame.self().getStartLocation
          val enemyStarts = nativeGame.getStartLocations.asScala.toVector.filterNot(_ == own)
            .map(t => MapTilePosition(t.x, t.y))
          huntPoints =
            (enemyStarts ++ HuntSweep.order(
              strategicMap.resources.map(_.nearbyFreeTile).toSeq,
              bases.mainBase.map(_.mainBuilding.tilePosition)
            )).distinct
        }
        huntTarget = if (huntPoints.isEmpty) None else Some(huntPoints(huntIndex % huntPoints.size))
        if (huntTarget.isDefined) huntIndex = HuntSweep.next(huntPoints.size, huntIndex)
        huntInitiated = false
        huntTarget.foreach(p =>
          NativeMatchEvidence.trace("hunt-sweep", s"point=$p index=$huntIndex areas=${huntPoints.size}")
        )
      }
    }
    val target = known.orElse(huntTarget)
    if (target != previousTarget) {
      worldDominationPlan.setCampaignTarget(target)
      NativeMatchEvidence.trace("campaign-target", target.toString)
      previousTarget = target
      launched = false
    }
    val troops   = expedition
    val minerals = troops.map(_.nativeUnitType.mineralPrice).sum
    val gas      = troops.map(_.nativeUnitType.gasPrice).sum
    if (!worldDominationPlan.planningInProgress && worldDominationPlan.campaignForceSize == 0) launched = false
    val ready = reconnaissanceAllowed
    if (ready && !wasReady) NativeMatchEvidence.trace(
      "offense-ready",
      s"establishedSecondBase=true fighters=${troops.size} army=$minerals/$gas bank=${resources.currentResources}"
    )
    if (!ready && target.isDefined && currentTick % 240 == 0) {
      val bunkers = universe.pluginByType[TerranBunkerDefense]
      NativeMatchEvidence.trace(
        "offense-gate",
        s"operational=${universe.pluginByType[ManageMiningAtBases].secondBaseEstablished} defense=${bunkers.defenseSufficient} coverage=${bunkers.coverageReady} pressure=${worldDominationPlan.baseDefenseActive} launched=$launched planning=${worldDominationPlan.planningInProgress} fighters=${troops.size} army=$minerals/$gas bank=${resources.currentResources} target=$target"
      )
    }
    wasReady = ready
    // A sealed depot wall must open once the army is ready, or no scout ever finds the target the launch needs.
    if (ready) {
      val wall = universe.pluginByType[WallWithDepots]
      if (strategy.current.usesWallDefense && wall.complete && !wall.gateOpen) wall.openGate_!()
    }
    if (campaignLaunchEnabled) target.foreach { where =>
      if (ready && !worldDominationPlan.planningInProgress) {
        // a walled main lets the army out: one wall depot is demolished to open the gate
        val wall = universe.pluginByType[WallWithDepots]
        if (strategy.current.usesWallDefense && wall.complete && !wall.gateOpen) wall.openGate_!()
        val accepted = worldDominationPlan.initiateCampaignAttack(where, troops.map(_.nativeUnitId).toSet)
        if (accepted) {
          if (huntTarget.contains(where)) huntInitiated = true
          campaignCommitted = true
          NativeMatchEvidence.trace(
            if (launched) "reinforce" else "launch",
            s"$where count=${troops.size} minerals=$minerals gas=$gas"
          )
          launched = true
        }
      }
    }
    if (currentTick % (31 * 16) == 0) {
      val mining       = universe.pluginByType[ManageMiningAtBases]
      val bunkers      = universe.pluginByType[TerranBunkerDefense]
      val funds        = resources.currentResources
      val overwhelming = troops.size >= config.minFighters * 2 &&
        minerals >= config.armyMinerals + config.bankMinerals &&
        gas >= config.armyGas + config.bankGas
      val mode =
        if (huntTarget.isDefined && known.isEmpty) "sweep"
        else if (target.isDefined) "attack"
        else "idle"
      NativeMatchEvidence.trace(
        "strategy-offense",
        s"operational=${mining.secondBaseEstablished} defense=${bunkers.defenseSufficient} coverage=${bunkers.coverageReady} pressure=${worldDominationPlan.baseDefenseActive} mode=$mode launched=$launched planning=${worldDominationPlan.planningInProgress} fighters=${troops.size}/${config.minFighters} armyMinerals=$minerals/${config.armyMinerals} armyGas=$gas/${config.armyGas} bankMinerals=${funds.minerals}/${config.bankMinerals} bankGas=${funds.gas}/${config.bankGas} overwhelming=$overwhelming target=$target"
      )
      NativeMatchEvidence.trace(
        "strategy-scouting",
        s"minimal=$minimalScoutingActive reconnaissance=$reconnaissanceAllowed enemyLocated=$enemyLocated"
      )
    }
  }
}
