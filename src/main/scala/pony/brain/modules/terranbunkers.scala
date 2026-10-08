package pony
package brain
package modules

import scala.jdk.CollectionConverters._
import scala.collection.mutable

private[pony] object BunkerCoverage {
  /** Match the existing mineral-path mask: every patch lane plus actual depot return approaches. */
  def workerTiles(patches: Seq[Area], depots: Seq[Area], routes: Seq[Seq[MapTilePosition]]): Vector[MapTilePosition] = {
    val tiles = mutable.Set.empty[MapTilePosition]
    patches.foreach(p => tiles ++= p.growBy(1).tiles)
    depots.foreach { depot =>
      tiles ++= depot.outline
      tiles ++= depot.growBy(1).outline
    }
    routes.foreach { path =>
      tiles ++= path
      path.sliding(2).foreach { pair => if (pair.size == 2)
        AreaHelper.traverseTilesOfLine(pair.head, pair.last, (x, y) => tiles += MapTilePosition(x, y)) }
    }
    tiles.toVector.sortBy(p => (p.y, p.x))
  }
  // BWAPI 4.1 Position.h: native approximation; ignoring collision extents is conservative.
  def approximateDistance(a: MapPosition, b: MapPosition): Int = {
    val dx = math.abs(a.x - b.x); val dy = math.abs(a.y - b.y)
    val small = dx min dy; val large = dx max dy
    if (small < (large >> 2)) large
    else { val m = (3 * small) >> 3; (m >> 5) + m + large - (large >> 4) - (large >> 6) }
  }
  def ready(points: Vector[MapPosition], sites: Vector[Area], completedCargo: Map[MapTilePosition, Int], range: Int) =
    sites.nonEmpty && sites.forall(s => completedCargo.get(s.upperLeft).contains(4)) &&
      points.forall(p => sites.exists(covers(_, p, range)))
  def corners(tiles: Seq[MapTilePosition]): Vector[MapPosition] = tiles.flatMap { t =>
    Seq(MapPosition(t.mapX, t.mapY), MapPosition(t.mapX + 32, t.mapY),
      MapPosition(t.mapX, t.mapY + 32), MapPosition(t.mapX + 32, t.mapY + 32))
  }.distinct.toVector
  def covers(site: Area, point: MapPosition, range: Int): Boolean = {
    val center = MapPosition(site.upperLeft.mapX + site.width * 16, site.upperLeft.mapY + site.height * 16)
    approximateDistance(center, point) <= range
  }
  private def separate(a: Area, b: Area) = !a.growBy(1).tiles.exists(b.tiles.toSet)
  private def overlaps(a: Area, b: Area) = a.tiles.exists(b.tiles.toSet)
  def select(points: Vector[MapPosition], candidates: Vector[Area], existing: Vector[Area], range: Int,
             safeTogether: Seq[Area] => Boolean = _ => true,
             relaxedFallback: Boolean = false): Vector[Area] = {
    val needed = points.indices.filterNot(i => existing.exists(covers(_, points(i), range))).toSet
    val ranked = candidates.filter(c => existing.forall(separate(c, _))).map { c =>
      c -> needed.filter(i => covers(c, points(i), range))
    }.filter(_._2.nonEmpty).sortBy { case (c, hit) => (-hit.size, c.upperLeft.y, c.upperLeft.x) }
    if (needed.isEmpty) Vector.empty
    else ranked.find(e => e._2 == needed && safeTogether(Vector(e._1))).map(e => Vector(e._1)).getOrElse {
      val pair = ranked.indices.iterator.flatMap { i =>
        val left = needed -- ranked(i)._2
        (i + 1 until ranked.size).iterator.filter(j => separate(ranked(i)._1, ranked(j)._1) &&
          left.subsetOf(ranked(j)._2) && safeTogether(Vector(ranked(i)._1, ranked(j)._1)))
          .map(j => Vector(ranked(i)._1, ranked(j)._1))
      }.take(1).toVector.headOption
      pair.getOrElse {
        def greedy(requireSafety: Boolean): Vector[Area] = {
          var left = needed
          var selected = Vector.empty[Area]
          while (left.nonEmpty) {
            // Preserve the same first admissible ranked site; whole-map connectivity is expensive.
            // The relaxed pass only forbids direct footprint overlap; tiles between cramped
            // mining lanes do not allow the full one-tile buffer on every start.
            def admissible(c: Area) =
              if (requireSafety) selected.forall(separate(c, _))
              else selected.forall(!overlaps(c, _))
            val options = ranked.filter(e => admissible(e._1))
              .map(e => (e._1, e._2 intersect left)).filter(_._2.nonEmpty)
              .sortBy(e => (-e._2.size, e._1.upperLeft.y, e._1.upperLeft.x))
            val next = if (requireSafety) options.iterator.find(e => safeTogether(selected :+ e._1))
              else options.headOption
            if (next.isEmpty) return Vector.empty
            selected :+= next.get._1
            left --= next.get._2
          }
          selected
        }
        val strict = greedy(requireSafety = true)
        if (strict.nonEmpty || !relaxedFallback) strict else greedy(requireSafety = false)
      }
    }
  }
}

private[pony] object BunkerWorkerRoutes {
  def between(from: MapTilePosition, to: MapTilePosition, grid: Grid2D): Option[Vector[MapTilePosition]] = {
    if (!grid.freeAndInBounds(from) || !grid.freeAndInBounds(to)) None
    else if (grid.connectedByLine(from, to)) Some(Vector(from, to))
    else new PathFinder(grid, true).findSimplePathNow(from, to, tryFixPath = false)
      .filter(_.solved).map(p => (Vector(from) ++ p.waypoints :+ to).distinct)
      .filter(route => route.sliding(2).forall(p => p.size < 2 || grid.connectedByLine(p.head, p.last)))
  }
}

/** Boarding intentions remain reserved; acceptance still requires four actual native cargo units. */
private[pony] class BunkerGarrison {
  private var assigned = Map.empty[Int, Vector[Int]]
  def update(bunkers: Seq[(Int, MapTilePosition, Set[Int])], marines: Seq[(Int, MapTilePosition)]): Unit = {
    val alive = marines.map(_._1).toSet
    val bunkerIds = bunkers.map(_._1).toSet
    assigned = assigned.filter(e => bunkerIds(e._1)).map { case (id, ids) => id -> ids.filter(alive).take(4) }
    val loaded = bunkers.flatMap(_._3).toSet
    assigned = assigned.map { case (id, ids) => id -> ids.filterNot(loaded) }
    bunkers.sortBy(_._1).foreach { case (id, tile, cargo) =>
      val kept = (cargo.toVector.sorted ++ assigned.getOrElse(id, Vector.empty)).distinct.take(4)
      val busy = assigned.values.flatten.toSet ++ loaded ++ kept
      val fill = marines.filterNot(m => busy(m._1)).sortBy(m => (m._2.distanceSquaredTo(tile), m._1))
        .take(4 - kept.size).map(_._1)
      assigned += id -> (kept ++ fill)
    }
  }
  def target(marine: Int) = assigned.collectFirst { case (id, ids) if ids.contains(marine) => id }
  def reserved = assigned.values.flatten.toSet
}

/** Retry refused boarding, but leave a progressing native approach untouched. */
private[pony] class BunkerBoardingRetry {
  private var lastAttempt = -1000
  private var lastProgress = 0
  private var lastPosition = Option.empty[MapTilePosition]
  def issue(frame: Int, position: MapTilePosition, loaded: Boolean, headingToBunker: Boolean, moving: Boolean): Boolean = {
    if (!lastPosition.contains(position)) { lastPosition = Some(position); lastProgress = frame }
    if (loaded || frame - lastAttempt < 12 || (headingToBunker && moving && frame - lastProgress < 120)) false
    else { lastAttempt = frame; true }
  }
}

/** Indexed dead cargo is not a survivor; visible training and its job are one slot. */
private[pony] object BunkerMarineQuota {
  def missing(seats: Int, nativeCompleted: Set[Int], nativeIncomplete: Set[Int],
              trainingJobs: Int, fundedRequests: Seq[Int], campaignHeld: Set[Int] = Set.empty,
              obsoleteCargo: Set[Int] = Set.empty): Int =
    (seats - (nativeCompleted -- campaignHeld -- obsoleteCargo).size -
      (nativeIncomplete.size max trainingJobs) - fundedRequests.sum) max 0
}

private[pony] object BunkerRepairAdmission {
  def eligible(damaged: Boolean, completed: Boolean, alive: Boolean, local: Boolean,
               mineralOrIdle: Boolean) = damaged && completed && alive && local && mineralOrIdle
}

private[pony] object BunkerRepairState {
  sealed trait State
  case object Repairing extends State
  case object Finished extends State
  case object Failed extends State
  def apply(workerAlive: Boolean, targetAlive: Boolean, damaged: Boolean, floating: Boolean): State =
    if (!workerAlive) Failed else if (!targetAlive || !damaged) Finished else if (floating) Failed else Repairing
}

/** Temporary custody steals only local mineral/idle SCVs, and returns them through the normal market. */
private[pony] class RepairDefensiveBunker(worker: SCV, bunker: Bunker, owner: Employer[SCV])
  extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString = s"Repair bunker ${bunker.nativeUnitId}"
  private def state = BunkerRepairState(worker.nativeUnit.exists && !worker.isDead, bunker.nativeUnit.exists,
    bunker.nativeUnit.getHitPoints < bunker.nativeUnit.getType.maxHitPoints, bunker.isFloating)
  override def isFinished = state == BunkerRepairState.Finished
  override def jobHasFailedWithoutDeath = state == BunkerRepairState.Failed
  override def everyNth = 23
  override def ordersForTick = Orders.RepairBuilding(worker, bunker).toSeq
  def targetId = bunker.nativeUnitId
}

class TerranBunkerDefense(universe: Universe)
  extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {
  private val ownedRequests = mutable.ArrayBuffer.empty[BuildUnitRequest[_ <: Building]]
  private val builder = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper {
    override protected def onBuildingRequested(request: BuildUnitRequest[_ <: Building]): Unit =
      if (request.typeOfRequestedUnit == classOf[Bunker]) ownedRequests += request
  }
  private val plans = mutable.Map.empty[Int, (Vector[MapPosition], Vector[Area])]
  private val geometry = mutable.Map.empty[Int, Vector[Area]]
  private val garrison = new BunkerGarrison
  private val repairers = new Employer[SCV](universe)
  private var cargoObserved = Map.empty[Int, Set[Int]]
  private var announcedReady = Set.empty[Int]
  private var uncoveredReported = Set.empty[Int]
  private var placementReported = Set.empty[MapTilePosition]
  private def active = race.isTerran && strategy.current.isInstanceOf[Strategy.SimpleTerran]
  private def bunkers = ownUnits.allByType[Bunker].filter(_.isInGame).toVector
  private def plannedSites = plans.values.flatMap(_._2.map(_.upperLeft)).toSet
  private def activeBunkers = bunkers.filter(b => plannedSites(b.tilePosition))
  private def nativeCargo(b: Bunker): Set[Int] = b.nativeUnit.getLoadedUnits.asScala.filter { u =>
    u.getType == bwapi.UnitType.Terran_Marine && u.isLoaded &&
      Option(u.getTransport).exists(_.getID == b.nativeUnitId)
  }.map(_.getID).toSet
  private val boarding = oncePerTick {
    if (active) {
      val ready = activeBunkers.filterNot(_.isBeingCreated)
      val slots = ready.map(b => (b.nativeUnitId, b.tilePosition, nativeCargo(b)))
      val nativeIds = nativeGame.self().getUnits.asScala.map(_.getID).toSet
      val activeCargo = ready.flatMap(nativeCargo).toSet
      val candidates = ownUnits.allByType[Marine].filter(m => nativeIds(m.nativeUnitId) && m.isInGame && !m.isBeingCreated &&
        (!m.nativeUnit.isLoaded || activeCargo(m.nativeUnitId)) &&
        !worldDominationPlan.attackOf(m).exists(_.campaign)).map(m => m.nativeUnitId -> m.currentTile).toVector
      garrison.update(slots, candidates)
    } else garrison.update(Nil, Nil)
    garrison
  }
  def reserved(m: WrapsUnit) = boarding.get.reserved(m.nativeUnitId) ||
    (m.isInstanceOf[Marine] && m.nativeUnit.isLoaded)
  def bunkerFor(m: Marine) = boarding.get.target(m.nativeUnitId).flatMap(id => bunkers.find(_.nativeUnitId == id))
  def coverageReady: Boolean = {
    if (!active) true
    else {
      val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
        .flatMap(_.resourceArea).map(_.uniqueId).toSet
      val completed = bunkers.filterNot(_.isBeingCreated).map(b => b.tilePosition -> nativeCargo(b).size).toMap
      val range = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
      fields.nonEmpty && fields.forall(id => plans.get(id).exists { case (points, sites) =>
        BunkerCoverage.ready(points, sites, completed, range)
      })
    }
  }
  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!active || currentTick < 31 || currentTick % 31 != 0) return
    boarding.get
    val range = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
    val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
      .flatMap(_.resourceArea).groupBy(_.uniqueId).values.map(_.head).toVector
    val fieldIds = fields.map(_.uniqueId).toSet
    plans.keys.filterNot(fieldIds).toVector.foreach { id => plans.remove(id); geometry.remove(id) }
    fields.foreach { field =>
      val depots = universe.pluginByType[ManageMiningAtBases].servingMineralDepots(field.uniqueId).map(_.area)
      val patches = field.patches.toList.flatMap(_.patches.map(_.area))
      val binding = (depots ++ patches).sortBy(a => (a.upperLeft.y, a.upperLeft.x))
      if (!plans.contains(field.uniqueId) || !geometry.get(field.uniqueId).contains(binding)) {
        plans.remove(field.uniqueId)
        val ground = mapLayers.freeWalkableIgnoringMobiles.guaranteeImmutability
        val routes = for (depot <- depots; patch <- patches) yield {
          val pairs = for (from <- depot.growBy(1).outline.filter(ground.freeAndInBounds);
                           to <- patch.growBy(1).outline.filter(ground.freeAndInBounds)) yield (from, to)
          pairs.toVector.sortBy(p => p._1.distanceSquaredTo(p._2)).iterator
            .map(p => BunkerWorkerRoutes.between(p._1, p._2, ground)).find(_.isDefined).flatten
        }
        // Tiles under completed or planned buildings can never host a worker or a bunker covering
        // them; keeping them as required points would deadlock re-planning after any construction.
        val workTiles = BunkerCoverage.workerTiles(patches, depots, routes.flatten)
          .filter(mapLayers.rawWalkableMap.insideBounds)
          .filter(mapLayers.blockedByBuildingTiles.free)
        val points = BunkerCoverage.corners(workTiles)
        val existing = bunkers.filter(b => b.tilePosition.distanceToIsLess(field.center, 15)).map(_.area)
        val finder = new ConstructionSiteFinder(universe)
        val routeTiles = BunkerCoverage.workerTiles(Nil, Nil, routes.flatten).toSet
        val candidates = finder.bunkerSites(field, workTiles).filterNot(a => a.tiles.exists(routeTiles))
        // Cramped terrain (map corners) may admit no jointly split-free set; then keep individual
        // coverage with separated sites instead of refusing to plan any bunkers at all.
        val sites = BunkerCoverage.select(points, candidates, existing, range,
          finder.bunkerSitesSafeTogether, relaxedFallback = true)
        if (routes.nonEmpty && routes.forall(_.isDefined) && points.nonEmpty &&
          (sites.nonEmpty || points.forall(p => existing.exists(BunkerCoverage.covers(_, p, range))))) {
          plans(field.uniqueId) = points -> (existing ++ sites)
          geometry(field.uniqueId) = binding
          announcedReady -= field.uniqueId
          NativeMatchEvidence.trace("bunker-coverage-plan", s"field=${field.uniqueId} range=$range model=conservativeNativeApprox points=${points.size} solvedRoutes=${routes.size} jointFootprintsSafe=true sites=${(existing ++ sites).map(_.upperLeft)}")
          uncoveredReported -= field.uniqueId
        } else if (!uncoveredReported(field.uniqueId)) {
          NativeMatchEvidence.trace("bunker-coverage-unavailable", s"field=${field.uniqueId} points=${points.size} safeCandidates=${candidates.size} noGenericFallback=true")
          val individuallyUncovered = points.filterNot(p => (existing ++ candidates).exists(BunkerCoverage.covers(_, p, range)))
          NativeMatchEvidence.trace("bunker-coverage-geometry", s"field=${field.uniqueId} range=$range requiredPoints=$points individuallyUncovered=$individuallyUncovered unsolvedRoutes=${routes.zipWithIndex.filter(_._1.isEmpty).map(_._2)} existing=${existing.map(_.upperLeft)} candidates=${candidates.map(_.upperLeft)}")
          uncoveredReported += field.uniqueId
        }
      }
      plans.get(field.uniqueId).foreach { case (_, sites) =>
        val pending = unitManager.requestedConstructions[Bunker].flatMap(_.customPosition.requestedPosition).toSet ++
          unitManager.constructionsInProgress[Bunker].map(_.buildWhere)
        sites.foreach { site =>
          if (!bunkers.exists(_.tilePosition == site.upperLeft) && !pending(site.upperLeft)) {
            val safe = new ConstructionSiteFinder(universe).bunkerSiteSafe(site)
            if (safe) {
              if (!placementReported(site.upperLeft)) {
                val now = nativeGame.canBuildHere(site.upperLeft.asTilePosition, bwapi.UnitType.Terran_Bunker)
                NativeMatchEvidence.trace("bunker-construction-request", s"field=${field.uniqueId} site=${site.upperLeft} nativeSpaceNow=$now error=n/a")
                placementReported += site.upperLeft
              }
              builder.requestBuilding(classOf[Bunker], takeCareOfDependencies = true,
                customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(site.upperLeft)(
                  new ConstructionSiteFinder(universe).bunkerSiteSafe(site)), belongsTo = Some(field))
            } else plans.remove(field.uniqueId)
          }
        }
      }
    }
    val activeSites = plannedSites
    ownedRequests.filter(r => !r.clearable && r.stillLocksResources && resources.hasStillLocked(r.funding) &&
      r.customPosition.requestedPosition.exists(p => !activeSites(p))).foreach { r =>
      r.forceUnlockOnDispose_!()
      r.clearableInNextTick_!()
      NativeMatchEvidence.trace("bunker-request-retired", s"site=${r.customPosition.requestedPosition}")
    }
    val ownedFunding = ownedRequests.map(_.funding).toSet
    unitManager.constructionsInProgress[Bunker].filter(j => ownedFunding(j.proofForFunding) &&
      !activeSites(j.buildWhere) && !j.unit.isConstructingBuilding && j.building.isEmpty &&
      ownUnits.buildingAt(j.buildWhere).isEmpty).foreach(_.fail_!())
    ownedRequests --= ownedRequests.filterNot(r => resources.hasStillLocked(r.funding))
    val desired = plans.values.map(_._2.size * 4).sum
    val nativeMarines = nativeGame.self().getUnits.asScala.filter(_.getType == bwapi.UnitType.Terran_Marine).toVector
    val expeditionMarines = ownUnits.allByType[Marine].filter(m =>
      worldDominationPlan.attackOf(m).exists(_.campaign)).map(_.nativeUnitId).toSet
    val nativeIds = nativeGame.self().getUnits.asScala.map(_.getID).toSet
    val training = unitManager.allJobsByType[TrainUnit[UnitFactory, Mobile]].count(j =>
      j.requestedType == classOf[Marine] && nativeIds(j.unit.nativeUnitId) && !j.failedOrObsolete && !j.isFinished)
    val funded = unitManager.plannedToTrain.filter(r => !r.clearable && r.typeOfRequestedUnit == classOf[Marine] &&
      r.funding.isSuccess && resources.hasStillLocked(r.funding)).map(_.amount).toVector
    val missing = BunkerMarineQuota.missing(desired, nativeMarines.filter(_.isCompleted).map(_.getID).toSet,
      nativeMarines.filterNot(_.isCompleted).map(_.getID).toSet, training, funded, expeditionMarines,
      bunkers.filterNot(b => activeSites(b.tilePosition)).flatMap(nativeCargo).toSet)
    if (missing > 0) {
      val accepted = requestUnit(classOf[Marine], takeCareOfDependencies = true, priority = Priority.Supply)
      if (currentTick % (31 * 16) == 0) NativeMatchEvidence.trace("bunker-replacement-request",
        s"desired=$desired native=${nativeMarines.size} indexed=${ownUnits.allByType[Marine].size} training=$training funded=$funded missing=$missing accepted=$accepted unlocked=${resources.unlockedResources}")
    }
    bunkers.filter(b => b.nativeUnit.exists && b.nativeUnit.isCompleted && b.nativeUnit.getHitPoints < b.nativeUnit.getType.maxHitPoints).foreach { b =>
      val assigned = unitManager.allJobsByType[RepairDefensiveBunker].count(j =>
        j.targetId == b.nativeUnitId && !j.failedOrObsolete && !j.isFinished)
      if (assigned < 2) {
        val request = UnitJobRequest.idleOfType(repairers, classOf[SCV], 2 - assigned, Priority.Supply)
          .withOnlyAccepting { w =>
            val job = unitManager.jobOf(w)
            BunkerRepairAdmission.eligible(b.isDamaged, !b.isBeingCreated, nativeIds(w.nativeUnitId),
              w.currentTile.distanceToIsLess(b.centerTile, 12) && w.currentArea.contains(b.areaOnMap),
              job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
          }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](b.centerTile)()))
        unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
          repairers.assignJob_!(new RepairDefensiveBunker(w, b, repairers))
          NativeMatchEvidence.trace("bunker-repair-assigned", s"bunker=${b.nativeUnitId} scv=${w.nativeUnitId} hp=${b.nativeUnit.getHitPoints}")
        }
      }
    }
    val cargo = bunkers.filterNot(_.isBeingCreated).map(b => b.nativeUnitId -> nativeCargo(b)).toMap
    if (currentTick % (31 * 16) == 0 && !coverageReady) {
      val eligible = ownUnits.allByType[Marine].filter(m => m.isInGame && !m.isBeingCreated).map(_.nativeUnitId).toVector
      val producers = ownUnits.allByType[Barracks].map(b => s"${b.nativeUnitId}:${b.nativeUnit.isTraining}:${b.nativeUnit.getRemainingTrainTime}:${unitManager.jobOf(b).shortDebugString}").toVector
      val assigned = bunkers.map(b => b.nativeUnitId -> boarding.get.reserved.toVector.filter(id => boarding.get.target(id).contains(b.nativeUnitId)))
      NativeMatchEvidence.trace("bunker-garrison-status", s"desired=$desired indexed=${ownUnits.allByType[Marine].size} nativeMarines=${nativeMarines.map(_.getID)} eligible=$eligible planned=${unitManager.plannedToTrain.count(_.typeOfRequestedUnit == classOf[Marine])} producers=$producers assigned=$assigned unlocked=${resources.unlockedResources}")
    }
    if (currentTick % (31 * 16) == 0) {
      val locks = resources.detailedLocks.groupBy(_.whatFor.className).map { case (kind, entries) =>
        s"$kind:${entries.size}:${entries.map(_.reqs.minerals).sum}/${entries.map(_.reqs.supply).sum}"
      }.toVector.sorted
      NativeMatchEvidence.trace("resource-reservations", s"bank=${resources.currentResources} locked=${resources.lockedResources} spendable=${resources.unlockedResources} holders=$locks fundedRequests=${unitManager.plannedToTrain.count(r => r.funding.isSuccess && resources.hasStillLocked(r.funding))} trainingJobs=${unitManager.allJobsByType[TrainUnit[UnitFactory, Mobile]].size}")
    }
    cargo.foreach { case (id, ids) =>
      if (!cargoObserved.get(id).contains(ids)) NativeMatchEvidence.trace("bunker-native-cargo", s"id=$id marines=${ids.toVector.sorted} count=${ids.size}")
    }
    cargoObserved = cargo
    plans.foreach { case (field, (points, sites)) =>
      val completed = bunkers.filterNot(_.isBeingCreated).map(b => b.tilePosition -> cargo.getOrElse(b.nativeUnitId, Set.empty).size).toMap
      val ready = BunkerCoverage.ready(points, sites, completed, range)
      if (ready && !announcedReady(field)) {
        NativeMatchEvidence.trace("bunker-field-ready", s"field=$field points=${points.size} bunkers=${sites.size} fourMarinesEach=true")
        announcedReady += field
      }
      if (!ready) announcedReady -= field
    }
  }
}
