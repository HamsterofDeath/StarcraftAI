package pony
package brain
package modules

/** Native observations drive the opening; time alone cannot establish an expansion. */
private[pony] object DepotRelocation {
  sealed trait Step
  case object AwaitSaturation extends Step
  case object FinishTraining  extends Step
  case object Lift            extends Step
  case object Fly             extends Step
  case object Land            extends Step
  case object Established     extends Step
  def next(
      saturated: Boolean,
      training: Boolean,
      lifted: Boolean,
      nearDestination: Boolean,
      landedAtDestination: Boolean
  ): Step = {
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
  private var relocation    = Option.empty[RelocateDepot]

  private def fieldUseful(area: ResourceArea): Boolean = {
    val initial = area.patches.map(_.initialValue).sum.toDouble
    val left    = area.patches.map(_.value).sum.toDouble
    initial <= 0 || TerranCampaignConfig.load().fieldUseful(left / initial)
  }

  /** The field the saturated opener would fly to: the nearest safe, useful one. */
  private def likelySecondField(home: Base): Option[ResourceArea] = {
    val homeTile = home.mainBuilding.tilePosition
    strategicMap.resources
      .filter { a =>
        !bases.isCovered(a) && fieldUseful(a) &&
        mapLayers.slightlyDangerousAsBlocked.free(a.nearbyFreeTile) &&
        unitGrid.enemy.allInRange[Mobile](a.nearbyFreeTile, 12).isEmpty &&
        mapLayers.freeWalkableIgnoringMobiles.areInSameWalkableArea(homeTile, a.nearbyFreeTile) &&
        strategicMap.defenseLineOf(a.nearbyFreeTile).isDefined
      }
      .toVector.sortBy(a => (a.nearbyFreeTile.distanceSquaredTo(homeTile), a.uniqueId))
      .headOption
  }

  override def onTick_!(): Unit = {
    if (currentTick % Primes.prime31.i != 0) return
    val mining = universe.pluginByType[ManageMiningAtBases]
    relocation = relocation.filterNot(j => j.failedOrObsolete || j.isFinished)
    val depots = ownUnits.allByType[CommandCenter].filter(_.isInGame).toVector
    bases.mainBase.foreach { home =>
      // The expander is the only place that knows the base limit. It compares the rich fields we
      // hold against the configured target and resolves a deficit with one decision: move the most
      // expendable depot for free, otherwise request a replacement when nothing can be spared.
      def fieldOf(cc: CommandCenter) =
        bases.allBases.find(_.mainBuilding == cc).flatMap(_.resourceArea)
      val landed         = depots.filterNot(cc => cc.isBeingCreated || cc.isFloating)
      val heldRichFields = landed.flatMap(fieldOf).filter(fieldUseful).map(_.uniqueId).distinct
      val deficit        = TerranCampaignConfig.load().requiredFields - heldRichFields.size
      val economyMoving  = mining.startingFieldSaturated || mining.secondBaseEstablished
      val expendable     = landed.filter { cc =>
        val deadField   = fieldOf(cc).forall(a => !fieldUseful(a))
        val sharedField = fieldOf(cc).exists(a => landed.count(o => o != cc && fieldOf(o).contains(a)) > 0)
        // The home depot anchors defense: it may only move off its own dead field.
        if (cc == home.mainBuilding) deadField else deadField || sharedField
      }.sortBy(cc => (if (fieldOf(cc).exists(fieldUseful)) 1 else 0, cc.nativeUnitId))
      if (deficit > 0 && economyMoving && relocation.isEmpty) {
        expendable.headOption match {
          case Some(cc) =>
            cc.relocating = true // let an already funded SCV finish, but do not start another queue.
            if (
              !cc.nativeUnit.isTraining && cc.nativeUnit.getRemainingTrainTime == 0 &&
              unitManager.jobOf(cc).isIdle && cc.nativeUnit.canLift()
            ) {
              val request = UnitJobRequest.idleOfType(
                depotEmployer,
                classOf[CommandCenter],
                priority = Priority.Expand
              ).withOnlyAccepting(_.nativeUnitId == cc.nativeUnitId)
              val accepted = unitManager.request(request)
              accepted.units.headOption.foreach { unit =>
                val job = new RelocateDepot(depotEmployer, unit, unit.tilePosition)
                depotEmployer.assignJob_!(job)
                relocation = Some(job)
              }
            }
          case None =>
            val cost  = ResourceRequests.forUnit(race, classOf[CommandCenter])
            val funds = resources.unlockedResources
            if (
              !unitManager.requestedToBuild(classOf[CommandCenter]) &&
              unitManager.constructionsInProgress[CommandCenter].isEmpty &&
              TerranCampaignConfig.load().expand(
                funds.minerals,
                funds.gas,
                cost.minerals,
                cost.gas,
                pending = false,
                safeReachableSite = true
              )
            ) {
              // Build the future flier as close as possible to the field it will later fly to, while
              // staying on the home side of the pairing rule so the depot still rebinds to the home
              // field and the relocation can trigger. Everything lazy or native is read here, on the
              // main thread; the background closure only reads captured values.
              val field      = likelySecondField(home)
              val homeTile   = home.mainBuilding.tilePosition
              val homeCenter = home.resourceArea.map(_.center)
              val custom     = field.map { target =>
                val targetTile   = target.nearbyFreeTile
                val targetCenter = target.center
                AlternativeBuildingSpot.fromExpensive(new ConstructionSiteFinder(universe)) { finder =>
                  val staysHomeField: Area => Boolean = homeCenter match {
                    case Some(hc) => area => area.distanceTo(hc) + 4.0 <= area.distanceTo(targetCenter)
                    case None     => _ => true
                  }
                  finder.findSpotFor(
                    homeTile,
                    classOf[CommandCenter],
                    preferNear = Some(targetTile),
                    acceptableArea = staysHomeField
                  )
                }
              }.getOrElse(AlternativeBuildingSpot.useDefault)
              requestBuilding(
                classOf[CommandCenter],
                customBuildingPosition = custom,
                belongsTo = home.resourceArea,
                priority = Priority.Expand
              )
              NativeMatchEvidence.trace(
                "home-depot-request",
                s"unlocked=${funds.minerals} toward=${field.map(_.uniqueId)}"
              )
            }
        }
      }
      if (currentTick % (Primes.prime31.i * 16) == 0) {
        val cfg     = TerranCampaignConfig.load()
        val cost    = ResourceRequests.forUnit(race, classOf[CommandCenter])
        val funds   = resources.unlockedResources
        val pending = unitManager.requestedToBuild(classOf[CommandCenter]) ||
          unitManager.constructionsInProgress[CommandCenter].nonEmpty
        val expandState =
          if (deficit <= 0) "held"
          else if (relocation.isDefined) "recycling"
          else if (pending) "building"
          else if (expendable.nonEmpty) "ready-to-move"
          else if (
            cfg.expand(
              funds.minerals,
              funds.gas,
              cost.minerals,
              cost.gas,
              pending = false,
              safeReachableSite = true
            )
          ) "requesting"
          else "waiting-for-minerals"
        val relocateState =
          if (relocation.isDefined) "recycling-depot"
          else if (deficit > 0 && expendable.nonEmpty) "pending-fresh-field"
          else if (deficit > 0) "awaiting-replacement"
          else "idle"
        NativeMatchEvidence.trace(
          "strategy-expansion",
          s"fields=${heldRichFields.size}/${cfg.requiredFields} ccs=${depots.size}/${cfg.requiredFields} deficit=$deficit expendable=${expendable.size} unlockedMinerals=${funds.minerals}/${cost.minerals +
              cfg.expansionReserve} pending=$pending saturated=${mining.startingFieldSaturated} secondBase=${mining.secondBaseEstablished} expand=$expandState relocate=$relocateState"
        )
      }
    }
  }

  class RelocateDepot(employer: Employer[CommandCenter], depot: CommandCenter, home: MapTilePosition)
      extends UnitWithJob[CommandCenter](employer, depot, Priority.Expand) {
    private var destination              = Option.empty[(ResourceArea, MapTilePosition)]
    private var phase                    = Option.empty[DepotRelocation.Step]
    private var lastTile                 = depot.tilePosition
    private var lastProgress             = currentTick
    private var landingRetries           = 0
    private var returningHome            = false
    private var chooseAttempts           = 0
    private def safe(area: ResourceArea) = !bases.isCovered(area) && fieldUseful(area) &&
      mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile) &&
      unitGrid.enemy.allInRange[Mobile](area.nearbyFreeTile, 12).isEmpty
    private def chooseDestination(): Unit = {
      returningHome = false
      // Prefer a field the current workers can still walk to, so the existing workforce keeps
      // mining. Terrain-only walkability ignores our own wall, so use the buildings-aware view:
      // after the wall stands, home workers cannot leave the base on foot at all. If nothing is
      // walkable, any safe field still works, because the relocated depot trains local miners.
      val walkable                             = mapLayers.freeWalkableIgnoringMobiles
      def walkableFromHere(area: ResourceArea) =
        walkable.areInSameWalkableArea(home, area.nearbyFreeTile)
      destination = strategicMap.resources.filter(safe)
        .filter(a => strategicMap.defenseLineOf(a.nearbyFreeTile).isDefined).toVector
        .sortBy(a =>
          (
            if (walkableFromHere(a)) 0 else 1,
            a.nearbyFreeTile.distanceSquaredTo(home),
            a.uniqueId
          )
        )
        .iterator.flatMap { area =>
          new ConstructionSiteFinder(this.universe).forResourceArea(area).find.map(area -> _)
        }
        .take(1).toList.headOption
      if (destination.isEmpty && depot.isFloating) {
        returningHome = true
        destination = bases.mainBase.flatMap(_.resourceArea).flatMap { field =>
          new ConstructionSiteFinder(this.universe).findSpotFor(home, classOf[CommandCenter], maxRange = 25)
            .map(field -> _)
        }
      }
    }
    override def everyNth                      = 31
    override def shortDebugString              = "Relocate depot: " + phase
    override def isFinished                    = phase.contains(DepotRelocation.Established)
    override def jobHasFailedWithoutDeath      = false
    override def onFinishOrFail(): Unit        = { super.onFinishOrFail(); depot.relocating = false }
    override def ordersForTick: Seq[UnitOrder] = {
      if (destination.isEmpty) {
        chooseAttempts += 1
        if (chooseAttempts > 8) {
          // No fresh field fits this flight; end the job so the expander can decide again.
          NativeMatchEvidence.trace("depot-relocation", s"id=${depot.nativeUnitId} phase=give-up")
          phase = Some(DepotRelocation.Established)
        } else chooseDestination()
      }
      destination.toList.flatMap { case (field, landingTile) =>
        val tile = depot.tilePosition
        if (tile != lastTile) { lastTile = tile; lastProgress = currentTick }
        val landedThere = !depot.isFloating && depot.nativeUnit.isCompleted && tile == landingTile
        val next        = DepotRelocation.next(
          true,
          depot.nativeUnit.isTraining,
          depot.isFloating,
          tile.distanceToIsLess(landingTile, 4),
          landedThere
        )
        if (!phase.contains(next)) NativeMatchEvidence.trace(
          "depot-relocation",
          s"id=${depot.nativeUnitId} phase=$next from=$tile to=$landingTile field=${field.uniqueId}"
        )
        phase = Some(next)
        next match {
          case DepotRelocation.Lift => if (depot.nativeUnit.canLift()) Orders.LiftDepot(depot).toSeq else Nil
          case DepotRelocation.Fly  =>
            if (!returningHome && (!safe(field) || currentTick - lastProgress > 24 * 120)) {
              destination = None
              lastProgress = currentTick
              Nil
            } else if (depot.nativeUnit.isMoving) Nil
            else Orders.FlyDepot(depot, landingTile).toSeq
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
