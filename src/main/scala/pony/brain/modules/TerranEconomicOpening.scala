package pony
package brain
package modules

/**
  * Build at home, use both production queues, and keep enough healthy mineral fields: a new command center, built at home
  * and flown to its field, is preferred over moving one that still mines.
  */
class TerranEconomicOpening(universe: Universe)
    extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private val depotEmployer = new Employer[CommandCenter](universe)
  private var relocation    = Option.empty[RelocateDepot]

  private def remaining(area: ResourceArea): Double = {
    val initial = area.patches.map(_.initialValue).sum.toDouble
    if (initial <= 0) 1.0 else area.patches.map(_.value).sum / initial
  }

  /** Not yet mined out. */
  private def fieldUseful(area: ResourceArea): Boolean = TerranCampaignConfig.load().fieldUseful(remaining(area))

  /** Rich enough to hold and to move to. */
  private def fieldHealthy(area: ResourceArea): Boolean = TerranCampaignConfig.load().fieldHealthy(remaining(area))

  /** Free, healthy fields ordered by ExpansionSite: our half of the map first, then the closest to our main base. */
  private def rankedFields(fields: Seq[ResourceArea]): Seq[ResourceArea] = {
    import scala.jdk.CollectionConverters._
    val own         = nativeGame.self().getStartLocation
    val ourStart    = (own.x, own.y)
    val enemyStarts = nativeGame.getStartLocations.asScala.toVector.filterNot(_ == own).map(t => (t.x, t.y))
    val main        = bases.mainBase.map(_.mainBuilding.tilePosition).fold(ourStart)(t => (t.x, t.y))
    val byId        = fields.map(a => a.uniqueId -> a).toMap
    ExpansionSite.rank(
      fields.map { a =>
        val tile = a.nearbyFreeTile
        ExpansionSite.Candidate(a.uniqueId, tile.x, tile.y, strategicMap.defenseLineOf(tile).isDefined)
      },
      main,
      ourStart,
      enemyStarts
    ).map(c => byId(c.id))
  }

  /** The field the next depot will fly to: the best ranked safe, healthy one. */
  private def likelySecondField(home: Base): Option[ResourceArea] =
    rankedFields(strategicMap.resources.filter { a =>
      !bases.isCovered(a) && fieldHealthy(a) &&
      mapLayers.slightlyDangerousAsBlocked.free(a.nearbyFreeTile) &&
      unitGrid.enemy.allInRange[Mobile](a.nearbyFreeTile, 12).isEmpty
    }.toVector).headOption

  override def onTick_!(): Unit = {
    if (currentTick % Primes.prime31.i != 0) return
    val mining = universe.pluginByType[ManageMiningAtBases]
    relocation = relocation.filterNot(j => j.failedOrObsolete || j.isFinished)
    val depots = ownUnits.allByType[CommandCenter].filter(_.isInGame).toVector
    bases.mainBase.foreach { home =>
      // The expander is the only place that knows the base limit. It compares the healthy fields we
      // hold against the configured target and resolves a deficit with one ExpansionChoice: fly a
      // freshly built depot to its field, otherwise build a new one, and move a depot off an
      // exhausted field only when no new one is coming.
      def fieldOf(cc: CommandCenter) =
        bases.allBases.find(_.mainBuilding == cc).flatMap(_.resourceArea)
      val cfg    = TerranCampaignConfig.load()
      val landed = depots.filterNot(cc => cc.isBeingCreated || cc.isFloating)
      // Fields count as held while healthy, so the next command center is under way before one runs dry.
      val heldRichFields = landed.flatMap(fieldOf).filter(fieldHealthy).map(_.uniqueId).distinct
      val heldStates     = mining.fieldStates.filter(s => heldRichFields.contains(s.id))
      val wanted         = ExpansionChoice.wantedFields(
        cfg.requiredFields,
        heldRichFields.size,
        heldStates.nonEmpty && heldStates.forall(_.saturated),
        cfg.maxFields
      )
      val deficit                        = wanted - heldRichFields.size
      val economyMoving                  = mining.startingFieldSaturated || mining.secondBaseEstablished
      def sharedField(cc: CommandCenter) =
        fieldOf(cc).exists(a => landed.count(o => o != cc && fieldOf(o).contains(a)) > 0)
      // A depot built at home shares the home field until it flies to its own; the home depot anchors defense and
      // only moves off its own exhausted field.
      val fresh     = landed.filter(cc => cc != home.mainBuilding && sharedField(cc))
      val exhausted = landed.filter(cc => fieldOf(cc).forall(a => !fieldUseful(a)))
      val cost      = ResourceRequests.forUnit(race, classOf[CommandCenter])
      val funds     = resources.unlockedResources
      // a depot still being built or flying to its field is under way as well
      val newUnderWay = unitManager.requestedToBuild(classOf[CommandCenter]) ||
        unitManager.constructionsInProgress[CommandCenter].nonEmpty ||
        depots.exists(cc => cc.isBeingCreated || cc.isFloating)
      val affordable =
        cfg.expand(funds.minerals, funds.gas, cost.minerals, cost.gas, pending = false, safeReachableSite = true)
      val choice = ExpansionChoice.decide(
        deficit,
        fresh.map(_.nativeUnitId),
        exhausted.map(_.nativeUnitId),
        newUnderWay,
        affordable
      )
      if (economyMoving && relocation.isEmpty) choice match {
        case ExpansionChoice.Move(id) =>
          depots.find(_.nativeUnitId == id).foreach { cc =>
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
          }
        case ExpansionChoice.BuildNew =>
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
        case _ =>
      }
      if (currentTick % (Primes.prime31.i * 16) == 0) {
        val expandState = choice match {
          case _ if relocation.isDefined       => "moving"
          case ExpansionChoice.Hold            => if (deficit <= 0) "held" else "building"
          case ExpansionChoice.Move(_)         => "ready-to-move"
          case ExpansionChoice.BuildNew        => "requesting"
          case ExpansionChoice.WaitForMinerals => "waiting-for-minerals"
        }
        NativeMatchEvidence.trace(
          "strategy-expansion",
          s"fields=${heldRichFields.size}/$wanted ccs=${depots.size} deficit=$deficit " +
            s"fresh=${fresh.size} exhausted=${exhausted.size} unlockedMinerals=${funds.minerals}/" +
            s"${cost.minerals + cfg.expansionReserve} newUnderWay=$newUnderWay " +
            s"saturated=${mining.startingFieldSaturated} secondBase=${mining.secondBaseEstablished} expand=$expandState"
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
    private def safe(area: ResourceArea) = !bases.isCovered(area) && fieldHealthy(area) &&
      mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile) &&
      unitGrid.enemy.allInRange[Mobile](area.nearbyFreeTile, 12).isEmpty
    private def chooseDestination(): Unit = {
      returningHome = false
      // The relocated depot trains local miners, so the field need not be walkable from home (after the
      // wall stands nothing is); walkability is only traced. ExpansionSite keeps the depot on our half.
      val walkable                             = mapLayers.freeWalkableIgnoringMobiles
      def walkableFromHere(area: ResourceArea) =
        walkable.areInSameWalkableArea(home, area.nearbyFreeTile)
      NativeMatchEvidence.trace(
        "depot-destination-candidates",
        s"depot=${depot.nativeUnitId} from=$home " + strategicMap.resources.toVector.map { a =>
          val tile = a.nearbyFreeTile
          s"${a.uniqueId}@$tile:d=${math.sqrt(tile.distanceSquaredTo(home).toDouble).toInt}" +
            s":covered=${bases.isCovered(a)}:healthy=${fieldHealthy(a)}" +
            s":danger=${!mapLayers.slightlyDangerousAsBlocked.free(tile)}" +
            s":enemies=${unitGrid.enemy.allInRange[Mobile](tile, 12).size}" +
            s":line=${strategicMap.defenseLineOf(tile).isDefined}:walk=${walkableFromHere(a)}"
        }.mkString(" ")
      )
      destination = rankedFields(strategicMap.resources.filter(safe).toVector)
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
