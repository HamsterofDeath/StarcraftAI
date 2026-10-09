package pony
package brain
package modules

import scala.jdk.CollectionConverters._
import scala.collection.mutable

class TerranBunkerDefense(universe: Universe)
    extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {
  private val ownedRequests = mutable.ArrayBuffer.empty[BuildUnitRequest[? <: Building]]
  private val builder       = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper {
    override protected def onBuildingRequested(request: BuildUnitRequest[? <: Building]): Unit =
      if (request.typeOfRequestedUnit == classOf[Bunker]) ownedRequests += request
  }
  private val plans             = mutable.Map.empty[Int, (Vector[MapPosition], Vector[Area])]
  private val geometry          = mutable.Map.empty[Int, Vector[Area]]
  private val garrison          = new BunkerGarrison
  private val repairers         = new Employer[SCV](universe)
  private var cargoObserved     = Map.empty[Int, Set[Int]]
  private var announcedReady    = Set.empty[Int]
  private var uncoveredReported = Set.empty[Int]
  private var placementReported = Set.empty[MapTilePosition]

  /**
    * When each site's bunker was requested, and since when workers have been sent to build it. A site whose bunker
    * has not appeared a game minute after the first worker was sent, or three game minutes after its request, cannot
    * be built (some sites stop every worker short of them) and is blocked.
    */
  private val requestedAt  = mutable.Map.empty[MapTilePosition, Int]
  private val workingSince = mutable.Map.empty[MapTilePosition, Int]
  private var blockedSites = Set.empty[MapTilePosition]
  private val SiteTimeout  = 24 * 180
  private val StuckAfter   = 24 * 60

  private def block(site: MapTilePosition, reason: String): Unit = if (!blockedSites(site)) {
    blockedSites += site
    // free the worker at once; the request is retired with the next plan check
    unitManager.constructionsInProgress[Bunker].filter(j => j.buildWhere == site && j.building.isEmpty).foreach(
      _.fail_!()
    )
    NativeMatchEvidence.trace(
      "bunker-site-blocked",
      s"site=$site reason=$reason requested=${requestedAt.get(site).map(currentTick - _).getOrElse(-1)} " +
        s"working=${workingSince.get(site).map(currentTick - _).getOrElse(-1)}"
    )
  }

  private def blockStuckSites(): Unit = {
    unitManager.constructionsInProgress[Bunker].foreach(job =>
      workingSince.getOrElseUpdate(job.buildWhere, currentTick)
    )
    workingSince.filterInPlace((site, _) => !bunkers.exists(_.tilePosition == site))
    workingSince.foreach { case (site, since) => if (currentTick - since > StuckAfter) block(site, "worker") }
  }

  /** The depots and patches of each field at which its last planning attempt found no plan. */
  private val unplannable                      = mutable.Map.empty[Int, Vector[Area]]
  private def active                           = race.isTerran && strategy.current.usesBunkerDefense
  private def bunkers                          = ownUnits.allByType[Bunker].filter(_.isInGame).toVector
  private def plannedSites                     = plans.values.flatMap(_._2.map(_.upperLeft)).toSet -- blockedSites
  private def activeBunkers                    = bunkers.filter(b => plannedSites(b.tilePosition))
  private def nativeCargo(b: Bunker): Set[Int] = b.nativeUnit.getLoadedUnits.asScala.filter { u =>
    u.getType == bwapi.UnitType.Terran_Marine && u.isLoaded &&
    Option(u.getTransport).exists(_.getID == b.nativeUnitId)
  }.map(_.getID).toSet
  private val boarding = oncePerTick {
    if (active) {
      val ready       = activeBunkers.filterNot(_.isBeingCreated)
      val slots       = ready.map(b => (b.nativeUnitId, b.tilePosition, nativeCargo(b)))
      val nativeIds   = nativeGame.self().getUnits.asScala.map(_.getID).toSet
      val activeCargo = ready.flatMap(nativeCargo).toSet
      val candidates  = ownUnits.allByType[Marine].filter(m =>
        nativeIds(m.nativeUnitId) && m.isInGame && !m.isBeingCreated &&
          (!m.nativeUnit.isLoaded || activeCargo(m.nativeUnitId)) &&
          !worldDominationPlan.attackOf(m).exists(_.campaign)
      ).map(m => m.nativeUnitId -> m.currentTile).toVector
      garrison.update(slots, candidates)
    } else garrison.update(Nil, Nil)
    garrison
  }
  def reserved(m: WrapsUnit) = boarding.get.reserved(m.nativeUnitId) ||
    (m.isInstanceOf[Marine] && m.nativeUnit.isLoaded)
  def bunkerFor(m: Marine)   = boarding.get.target(m.nativeUnitId).flatMap(id => bunkers.find(_.nativeUnitId == id))
  def coverageReady: Boolean = {
    if (!active) true
    else {
      val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
        .flatMap(_.resourceArea).map(_.uniqueId).toSet
      val completed = bunkers.filterNot(_.isBeingCreated).map(b => b.tilePosition -> nativeCargo(b).size).toMap
      val range     = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
      fields.nonEmpty && fields.forall(id =>
        plans.get(id).exists { case (points, sites) =>
          BunkerCoverage.ready(points, sites, completed, range)
        }
      )
    }
  }

  /** Attack entry gate: every landed field planned and at least one base fully fortified. */
  def defenseSufficient: Boolean = {
    if (!active) true
    else {
      val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
        .flatMap(_.resourceArea).map(_.uniqueId).toSet
      val completed = bunkers.filterNot(_.isBeingCreated).map(b => b.tilePosition -> nativeCargo(b).size).toMap
      val range     = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
      fields.nonEmpty && fields.forall(plans.contains) &&
      fields.exists(id =>
        plans.get(id).exists { case (points, sites) =>
          BunkerCoverage.ready(points, sites, completed, range)
        }
      )
    }
  }
  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!active) return
    // cheap, so on every module tick: a stuck site must not hold up the bunker queue for the 31-tick planning cadence
    blockStuckSites()
    if (currentTick < 31 || currentTick % 31 != 0) return
    boarding.get
    val range  = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
    val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
      .flatMap(_.resourceArea).groupBy(_.uniqueId).values.map(_.head).toVector
    val fieldIds = fields.map(_.uniqueId).toSet
    plans.keys.filterNot(fieldIds).toVector.foreach { id => plans.remove(id); geometry.remove(id) }
    var requestedThisTick = false
    var plannedThisTick   = false
    fields.foreach { field =>
      val depots  = universe.pluginByType[ManageMiningAtBases].servingMineralDepots(field.uniqueId).map(_.area)
      val patches = field.patches.toList.flatMap(_.patches.map(_.area))
      val binding = (depots ++ patches).sortBy(a => (a.upperLeft.y, a.upperLeft.x))
      // Planning a field costs up to seconds: plan at most one field per tick, and retry a field that could not be
      // planned only once its depots or patches change.
      val stale = !plans.contains(field.uniqueId) || !geometry.get(field.uniqueId).contains(binding)
      if (stale && !plannedThisTick && !unplannable.get(field.uniqueId).contains(binding))
        CpuProfile.time("bunker-plan") {
          plannedThisTick = true
          plans.remove(field.uniqueId)
          val ground = mapLayers.freeWalkableIgnoringMobiles.guaranteeImmutability
          val routes = for (depot <- depots; patch <- patches) yield {
            val pairs = for (
              from <- depot.growBy(1).outline.filter(ground.freeAndInBounds);
              to   <- patch.growBy(1).outline.filter(ground.freeAndInBounds)
            ) yield (from, to)
            pairs.toVector.sortBy(p => p._1.distanceSquaredTo(p._2)).iterator
              .map(p => BunkerWorkerRoutes.between(p._1, p._2, ground)).find(_.isDefined).flatten
          }
          // Tiles under completed or planned buildings can never host a worker or a bunker covering
          // them; keeping them as required points would deadlock re-planning after any construction.
          val workTiles = BunkerCoverage.workerTiles(patches, depots, routes.flatten)
            .filter(mapLayers.rawWalkableMap.insideBounds)
            .filter(mapLayers.blockedByBuildingTiles.free)
          val allPoints = BunkerCoverage.corners(workTiles)
          // Already completed and in-flight bunkers count as existing coverage so re-planning keeps
          // them instead of retiring construction the moment the binding changes.
          val existing = {
            val completed = bunkers.filter(b => b.tilePosition.distanceToIsLess(field.center, 15)).map(_.area)
            val inFlight  =
              (unitManager.requestedConstructions[Bunker].flatMap(_.customPosition.requestedPosition) ++
                unitManager.constructionsInProgress[Bunker].map(_.buildWhere))
                .filter(_.distanceToIsLess(field.center, 15)).map(p => Area(p, Size(3, 2)))
            (completed ++ inFlight).distinct
          }
          val finder     = new ConstructionSiteFinder(universe)
          val routeTiles = BunkerCoverage.workerTiles(Nil, Nil, routes.flatten).toSet
          // A bunker must never claim the footprint reserved for a (re)built or relocating CommandCenter.
          val depotFootprints =
            (unitManager.requestedConstructions[CommandCenter].flatMap(_.customPosition.requestedPosition) ++
              unitManager.constructionsInProgress[CommandCenter].map(_.buildWhere)).map(p => Area(p, Size(4, 3)))
          // A worker must be able to walk up to the site: BWAPI's path check ignores minerals, so a pocket behind the
          // mineral line passes canBuildHere and still never gets its bunker.
          val depotSides = depots.flatMap(_.growBy(1).outline.filter(ground.freeAndInBounds))
          // BWAPI's own path check works at walk-tile resolution and sees cliff edges the tile grid joins.
          def reachable(site: Area): Boolean = site.growBy(1).outline.filter(ground.freeAndInBounds).exists(t =>
            depotSides.exists(ground.areInSameWalkableArea(_, t))
          ) && depots.forall(d =>
            nativeGame.hasPath(d.center.toNative, site.center.toNative)
          )
          // Behind the mineral line (farther from the depot than every patch) a builder has to squeeze past the mining
          // workers and usually stalls; such sites are no candidates.
          def behindMinerals(site: Area) = depots.exists { d =>
            val reach = patches.map(_.centerTile.distanceSquaredTo(d.centerTile)).maxOption.getOrElse(0)
            site.centerTile.distanceSquaredTo(d.centerTile) > reach
          }
          val candidates = finder.bunkerSites(field, workTiles)
            .filterNot(a => blockedSites(a.upperLeft))
            .filterNot(behindMinerals)
            // a site a worker already failed to reach (or one overlapping it) is no candidate either
            .filterNot(a =>
              unitManager.unreachableSites.exists(u =>
                u.upperLeft.x <= a.lowerRight.x && a.upperLeft.x <= u.lowerRight.x &&
                  u.upperLeft.y <= a.lowerRight.y && a.upperLeft.y <= u.lowerRight.y
              )
            )
            .filter(reachable)
            .filterNot(a => a.tiles.exists(routeTiles))
            .filterNot(a => depotFootprints.exists(cc => a.growBy(1).tiles.exists(cc.tiles.toSet)))
          // Points no candidate footprint can reach (map edge lanes, ground occupied by other
          // buildings) must not demand coverage, or the field could never be planned at all.
          val points = allPoints.filter(p => (existing ++ candidates).exists(BunkerCoverage.covers(_, p, range)))
          // Cramped terrain (map corners) may admit no jointly split-free set; then keep individual
          // coverage with separated sites instead of refusing to plan any bunkers at all.
          val sites = BunkerCoverage.select(
            points,
            candidates,
            existing,
            range,
            finder.bunkerSitesSafeTogether,
            relaxedFallback = true
          )
          if (
            routes.nonEmpty && routes.forall(_.isDefined) && points.nonEmpty &&
            (sites.nonEmpty || points.forall(p => existing.exists(BunkerCoverage.covers(_, p, range))))
          ) {
            val allSites = existing ++ sites
            // Points a finished building already occupies can never be covered again; store only
            // what the final site set covers, or readiness would stay false forever.
            val covered = points.filter(p => allSites.exists(BunkerCoverage.covers(_, p, range)))
            plans(field.uniqueId) = covered -> allSites
            geometry(field.uniqueId) = binding
            unplannable.remove(field.uniqueId)
            announcedReady -= field.uniqueId
            NativeMatchEvidence.trace(
              "bunker-coverage-plan",
              s"field=${field.uniqueId} range=$range model=conservativeNativeApprox points=${covered.size}/${points.size} solvedRoutes=${routes.size} jointFootprintsSafe=true sites=${allSites.map(_.upperLeft)}"
            )
            uncoveredReported -= field.uniqueId
          } else if (!uncoveredReported(field.uniqueId)) {
            NativeMatchEvidence.trace(
              "bunker-coverage-unavailable",
              s"field=${field.uniqueId} points=${points.size} safeCandidates=${candidates.size} noGenericFallback=true"
            )
            val individuallyUncovered =
              points.filterNot(p => (existing ++ candidates).exists(BunkerCoverage.covers(_, p, range)))
            NativeMatchEvidence.trace(
              "bunker-coverage-geometry",
              s"field=${field.uniqueId} range=$range requiredPoints=$points individuallyUncovered=$individuallyUncovered unsolvedRoutes=${routes.zipWithIndex.filter(_._1.isEmpty).map(_._2)} existing=${existing.map(_.upperLeft)} candidates=${candidates.map(_.upperLeft)}"
            )
            uncoveredReported += field.uniqueId
          }
          if (!plans.contains(field.uniqueId)) unplannable(field.uniqueId) = binding
        }
      plans.get(field.uniqueId).foreach { case (_, sites) =>
        // One bunker at a time: several requested at once lock their price long before a worker gets to them and
        // starve the expansion.
        def bunkerUnderWay = requestedThisTick || unitManager.requestedConstructions[Bunker].nonEmpty ||
          unitManager.constructionsInProgress[Bunker].nonEmpty || bunkers.exists(_.isBeingCreated)
        val pending = unitManager.requestedConstructions[Bunker].flatMap(_.customPosition.requestedPosition).toSet ++
          unitManager.constructionsInProgress[Bunker].map(_.buildWhere)
        // A requested bunker that never appears must not block every other site.
        sites.filter(s => pending(s.upperLeft) && !bunkers.exists(_.tilePosition == s.upperLeft)).foreach { site =>
          if (requestedAt.get(site.upperLeft).exists(currentTick - _ > SiteTimeout)) block(site.upperLeft, "request")
        }
        sites.filterNot(s => blockedSites(s.upperLeft)).foreach { site =>
          if (!bunkers.exists(_.tilePosition == site.upperLeft) && !pending(site.upperLeft) && !bunkerUnderWay) {
            val safe = new ConstructionSiteFinder(universe).bunkerSiteSafe(site)
            if (safe) {
              if (!placementReported(site.upperLeft)) {
                val now = nativeGame.canBuildHere(site.upperLeft.asTilePosition, bwapi.UnitType.Terran_Bunker)
                NativeMatchEvidence.trace(
                  "bunker-construction-request",
                  s"field=${field.uniqueId} site=${site.upperLeft} nativeSpaceNow=$now"
                )
                placementReported += site.upperLeft
              }
              builder.requestBuilding(
                classOf[Bunker],
                takeCareOfDependencies = true,
                customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(site.upperLeft)(
                  new ConstructionSiteFinder(universe).bunkerSiteSafe(site)
                ),
                belongsTo = Some(field)
              )
              requestedThisTick = true
              requestedAt(site.upperLeft) = currentTick
            } else plans.remove(field.uniqueId)
          }
        }
      }
    }
    val activeSites = plannedSites
    ownedRequests.filter(r =>
      !r.clearable && r.stillLocksResources && resources.hasStillLocked(r.funding) &&
        r.customPosition.requestedPosition.exists(p => !activeSites(p))
    ).foreach { r =>
      r.forceUnlockOnDispose_!()
      r.clearableInNextTick_!()
      NativeMatchEvidence.trace("bunker-request-retired", s"site=${r.customPosition.requestedPosition}")
    }
    val ownedFunding = ownedRequests.map(_.funding).toSet
    unitManager.constructionsInProgress[Bunker].filter(j =>
      ownedFunding(j.proofForFunding) &&
        !activeSites(j.buildWhere) && !j.unit.isConstructingBuilding && j.building.isEmpty &&
        ownUnits.buildingAt(j.buildWhere).isEmpty
    ).foreach(_.fail_!())
    ownedRequests --= ownedRequests.filterNot(r => resources.hasStillLocked(r.funding))
    val desired       = plans.values.map(_._2.size * 4).sum
    val nativeMarines = nativeGame.self().getUnits.asScala.filter(_.getType == bwapi.UnitType.Terran_Marine).toVector
    val expeditionMarines = ownUnits.allByType[Marine].filter(m =>
      worldDominationPlan.attackOf(m).exists(_.campaign)
    ).map(_.nativeUnitId).toSet
    val nativeIds = nativeGame.self().getUnits.asScala.map(_.getID).toSet
    val training  = unitManager.allJobsByType[TrainUnit[UnitFactory, Mobile]].count(j =>
      j.requestedType == classOf[Marine] && nativeIds(j.unit.nativeUnitId) && !j.failedOrObsolete && !j.isFinished
    )
    val funded = unitManager.plannedToTrain.filter(r =>
      !r.clearable && r.typeOfRequestedUnit == classOf[Marine] &&
        r.funding.isSuccess && resources.hasStillLocked(r.funding)
    ).map(_.amount).toVector
    val missing = BunkerMarineQuota.missing(
      desired,
      nativeMarines.filter(_.isCompleted).map(_.getID).toSet,
      nativeMarines.filterNot(_.isCompleted).map(_.getID).toSet,
      training,
      funded,
      expeditionMarines,
      bunkers.filterNot(b => activeSites(b.tilePosition)).flatMap(nativeCargo).toSet
    )
    if (missing > 0) {
      val accepted = requestUnit(classOf[Marine], takeCareOfDependencies = true, priority = Priority.Supply)
      if (currentTick % (31 * 16) == 0) NativeMatchEvidence.trace(
        "bunker-replacement-request",
        s"desired=$desired native=${nativeMarines.size} indexed=${ownUnits.allByType[Marine].size} training=$training funded=$funded missing=$missing accepted=$accepted unlocked=${resources.unlockedResources}"
      )
    }
    bunkers.filter(b =>
      b.nativeUnit.exists && b.nativeUnit.isCompleted && b.nativeUnit.getHitPoints < b.nativeUnit.getType.maxHitPoints
    ).foreach { b =>
      val assigned = unitManager.allJobsByType[RepairDefensiveBunker].count(j =>
        j.targetId == b.nativeUnitId && !j.failedOrObsolete && !j.isFinished
      )
      if (assigned < 2) {
        val request = UnitJobRequest.idleOfType(repairers, classOf[SCV], 2 - assigned, Priority.Supply)
          .withOnlyAccepting { w =>
            val job = unitManager.jobOf(w)
            BunkerRepairAdmission.eligible(
              b.isDamaged,
              !b.isBeingCreated,
              nativeIds(w.nativeUnitId),
              w.currentTile.distanceToIsLess(b.centerTile, 12) && w.currentArea.contains(b.areaOnMap),
              job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch]
            )
          }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](b.centerTile)()))
        unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
          repairers.assignJob_!(new RepairDefensiveBunker(w, b, repairers))
          NativeMatchEvidence.trace(
            "bunker-repair-assigned",
            s"bunker=${b.nativeUnitId} scv=${w.nativeUnitId} hp=${b.nativeUnit.getHitPoints}"
          )
        }
      }
    }
    val cargo = bunkers.filterNot(_.isBeingCreated).map(b => b.nativeUnitId -> nativeCargo(b)).toMap
    if (currentTick % (31 * 16) == 0 && !coverageReady) {
      val eligible =
        ownUnits.allByType[Marine].filter(m => m.isInGame && !m.isBeingCreated).map(_.nativeUnitId).toVector
      val producers = ownUnits.allByType[Barracks].map(b =>
        s"${b.nativeUnitId}:${b.nativeUnit.isTraining}:${b.nativeUnit.getRemainingTrainTime}:${unitManager.jobOf(b).shortDebugString}"
      ).toVector
      val assigned = bunkers.map(b =>
        b.nativeUnitId -> boarding.get.reserved.toVector.filter(id => boarding.get.target(id).contains(b.nativeUnitId))
      )
      NativeMatchEvidence.trace(
        "bunker-garrison-status",
        s"desired=$desired indexed=${ownUnits.allByType[Marine].size} nativeMarines=${nativeMarines.map(_.getID)} eligible=$eligible planned=${unitManager.plannedToTrain.count(_.typeOfRequestedUnit ==
            classOf[Marine])} producers=$producers assigned=$assigned unlocked=${resources.unlockedResources}"
      )
    }
    if (currentTick % (31 * 16) == 0) {
      val locks = resources.detailedLocks.groupBy(_.whatFor.className).map { case (kind, entries) =>
        s"$kind:${entries.size}:${entries.map(_.reqs.minerals).sum}/${entries.map(_.reqs.supply).sum}"
      }.toVector.sorted
      NativeMatchEvidence.trace(
        "resource-reservations",
        s"bank=${resources.currentResources} locked=${resources.lockedResources} spendable=${resources.unlockedResources} holders=$locks fundedRequests=${unitManager.plannedToTrain.count(
            r => r.funding.isSuccess && resources.hasStillLocked(r.funding)
          )} trainingJobs=${unitManager.allJobsByType[TrainUnit[UnitFactory, Mobile]].size}"
      )
      val completedBunkers = bunkers.count(b => b.nativeUnit.exists && b.nativeUnit.isCompleted)
      NativeMatchEvidence.trace(
        "strategy-defense",
        s"plannedFields=${plans.size} sites=${plans.values.map(_._2.size).sum} completedBunkers=$completedBunkers desiredMarines=$desired loadedMarines=${cargo.values.map(_.size).sum} coverage=$coverageReady sufficient=$defenseSufficient"
      )
    }
    cargo.foreach { case (id, ids) =>
      if (!cargoObserved.get(id).contains(ids))
        NativeMatchEvidence.trace("bunker-native-cargo", s"id=$id marines=${ids.toVector.sorted} count=${ids.size}")
    }
    cargoObserved = cargo
    plans.foreach { case (field, (points, sites)) =>
      val completed = bunkers.filterNot(_.isBeingCreated).map(b =>
        b.tilePosition -> cargo.getOrElse(b.nativeUnitId, Set.empty).size
      ).toMap
      val ready = BunkerCoverage.ready(points, sites, completed, range)
      if (ready && !announcedReady(field)) {
        NativeMatchEvidence.trace(
          "bunker-field-ready",
          s"field=$field points=${points.size} bunkers=${sites.size} fourMarinesEach=true"
        )
        announcedReady += field
      }
      if (!ready) announcedReady -= field
    }
  }
}
