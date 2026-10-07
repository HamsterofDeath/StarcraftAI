package pony
package brain
package modules

/** Native observations drive the opening; time alone cannot establish an expansion. */
private[pony] object DepotRelocation {
  sealed trait Step
  case object AwaitSaturation extends Step
  case object FinishTraining extends Step
  case object Lift extends Step
  case object Fly extends Step
  case object Land extends Step
  case object Established extends Step
  def next(saturated: Boolean, training: Boolean, lifted: Boolean,
           nearDestination: Boolean, landedAtDestination: Boolean): Step = {
    if (landedAtDestination) Established
    else if (!saturated && !lifted) AwaitSaturation
    else if (training && !lifted) FinishTraining
    else if (!lifted) Lift
    else if (!nearDestination) Fly
    else Land
  }
}

/** Build at home, use both production queues, then move the spare depot after mineral saturation. */
class TerranEconomicOpening(universe: Universe)
  extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private val depotEmployer = new Employer[CommandCenter](universe)
  private var relocation = Option.empty[RelocateDepot]
  override def onTick_!(): Unit = {
    if (currentTick % Primes.prime31.i != 0) return
    val mining = universe.pluginByType[ManageMiningAtBases]
    relocation = relocation.filterNot(j => j.failedOrObsolete || j.isFinished)
    val depots = ownUnits.allByType[CommandCenter].filter(_.isInGame).toVector
    bases.mainBase.foreach { home =>
      if (depots.size < 2 && !unitManager.requestedToBuild(classOf[CommandCenter]) &&
        unitManager.constructionsInProgress[CommandCenter].isEmpty) {
        val cost = ResourceRequests.forUnit(race, classOf[CommandCenter])
        val funds = resources.unlockedResources
        if (TerranCampaignConfig.load().expand(funds.minerals, funds.gas, cost.minerals, cost.gas,
          pending = false, safeReachableSite = true)) {
          requestBuilding(classOf[CommandCenter], belongsTo = home.resourceArea, priority = Priority.Expand)
          NativeMatchEvidence.trace("home-depot-request", s"unlocked=${funds.minerals}")
        }
      }
      if (!mining.secondBaseOperational && mining.startingFieldSaturated && relocation.isEmpty) {
        val spare = depots.filterNot(_ == home.mainBuilding).find { cc =>
          !cc.isBeingCreated && bases.allBases.find(_.mainBuilding == cc).exists(_.resourceArea == home.resourceArea)
        }
        spare.foreach { cc =>
          cc.relocating = true // let an already funded SCV finish, but do not start another queue.
          if (!cc.nativeUnit.isTraining && cc.nativeUnit.getRemainingTrainTime == 0 &&
            unitManager.jobOf(cc).isIdle && cc.nativeUnit.canLift()) {
            val request = UnitJobRequest.idleOfType(depotEmployer, classOf[CommandCenter],
              priority = Priority.Expand).withOnlyAccepting(_.nativeUnitId == cc.nativeUnitId)
            val accepted = unitManager.request(request)
            accepted.units.headOption.foreach { unit =>
              val job = new RelocateDepot(depotEmployer, unit, home.mainBuilding.tilePosition)
              depotEmployer.assignJob_!(job)
              relocation = Some(job)
            }
          }
        }
      }
    }
  }

  class RelocateDepot(employer: Employer[CommandCenter], depot: CommandCenter, home: MapTilePosition)
    extends UnitWithJob[CommandCenter](employer, depot, Priority.Expand) {
    private var destination = Option.empty[(ResourceArea, MapTilePosition)]
    private var phase = Option.empty[DepotRelocation.Step]
    private var lastTile = depot.tilePosition
    private var lastProgress = currentTick
    private var landingRetries = 0
    private var returningHome = false
    private def safe(area: ResourceArea) = !bases.isCovered(area) && area.patches.exists(_.patches.exists(_.remaining > 0)) &&
      mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile) &&
      unitGrid.enemy.allInRange(area.nearbyFreeTile, 12).isEmpty &&
      mapLayers.rawWalkableMap.areInSameWalkableArea(home, area.nearbyFreeTile)
    private def unsafeBecause(area: ResourceArea) = {
      Seq(
        "covered" -> bases.isCovered(area),
        "depleted" -> !area.patches.exists(_.patches.exists(_.remaining > 0)),
        "blocked" -> !mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile),
        "enemyNear" -> unitGrid.enemy.allInRange(area.nearbyFreeTile, 12).nonEmpty,
        "otherArea" -> !mapLayers.rawWalkableMap.areInSameWalkableArea(home, area.nearbyFreeTile)
      ).collect { case (name, true) => name }.mkString(",")
    }
    private def chooseDestination(): Unit = {
      destination = strategicMap.resources.filter(safe).toVector.sortBy(_.nearbyFreeTile.distanceSquaredTo(home))
        .iterator.flatMap { area => new ConstructionSiteFinder(universe).forResourceArea(area).find.map(area -> _) }
        .take(1).toList.headOption
      if (destination.isEmpty && depot.isFloating) {
        returningHome = true
        destination = bases.mainBase.flatMap(_.resourceArea).map(_ -> home)
      }
      NativeMatchEvidence.trace("relocation-destination",
        s"id=${depot.nativeUnitId} from=${depot.tilePosition} to=${destination.map(_._2)} returningHome=$returningHome")
    }
    override def everyNth = 31
    override def shortDebugString = "Relocate depot: " + phase
    override def isFinished = phase.contains(DepotRelocation.Established)
    override def jobHasFailedWithoutDeath = false
    override def onFinishOrFail(): Unit = { super.onFinishOrFail(); depot.relocating = false }
    override def ordersForTick: Seq[UnitOrder] = {
      if (destination.isEmpty) chooseDestination()
      destination.toList.flatMap { case (field, landingTile) =>
        val tile = depot.tilePosition
        if (tile != lastTile) { lastTile = tile; lastProgress = currentTick }
        val landedThere = !depot.isFloating && tile == landingTile
        val next = DepotRelocation.next(saturated = true, depot.nativeUnit.isTraining,
          depot.isFloating, tile.distanceToIsLess(landingTile, 4), landedThere)
        if (!phase.contains(next)) NativeMatchEvidence.trace("depot-relocation",
          s"id=${depot.nativeUnitId} phase=$next from=$tile to=$landingTile field=${field.uniqueId}")
        phase = Some(next)
        next match {
          case DepotRelocation.Lift => if (depot.nativeUnit.canLift()) Orders.LiftDepot(depot).toSeq else Nil
          case DepotRelocation.Fly =>
            val unsafeNow = !safe(field)
            val stalled = currentTick - lastProgress > 24 * 120
            if (!returningHome && (unsafeNow || stalled)) {
              NativeMatchEvidence.trace("relocation-abort",
                s"id=${depot.nativeUnitId} tile=$tile target=$landingTile unsafe=$unsafeNow stalled=$stalled reasons=${unsafeBecause(field)}")
              destination = None
              lastProgress = currentTick
              Nil
            } else Orders.FlyDepot(depot, landingTile).toSeq
          case DepotRelocation.Land =>
            if (depot.nativeUnit.canLand(landingTile.asTilePosition)) {
              landingRetries = 0
              Orders.LandDepot(depot, landingTile).toSeq
            } else {
              landingRetries += 1
              if (landingRetries >= 8) {
                // Units can temporarily occupy the footprint. Search nearby instead of stranding a lifted depot.
                val alternatives = mapLayers.rawWalkableMap.spiralAround(landingTile, 8)
                alternatives.find(p => depot.nativeUnit.canLand(p.asTilePosition)).foreach { alternate =>
                  destination = Some(field -> alternate)
                }
                if (landingRetries >= 32) { destination = None; landingRetries = 0 }
              }
              Nil
            }
          case _ => Nil
        }
      }
    }
  }
}
