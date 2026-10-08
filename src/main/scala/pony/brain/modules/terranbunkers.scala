package pony
package brain
package modules

import scala.collection.JavaConverters._
import scala.collection.mutable

private[pony] object BunkerCoverage {
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
  def select(points: Vector[MapPosition], candidates: Vector[Area], existing: Vector[Area], range: Int): Vector[Area] = {
    val needed = points.indices.filterNot(i => existing.exists(covers(_, points(i), range))).toSet
    val ranked = candidates.filter(c => existing.forall(separate(c, _))).map { c =>
      c -> needed.filter(i => covers(c, points(i), range))
    }.filter(_._2.nonEmpty).sortBy { case (c, hit) => (-hit.size, c.upperLeft.y, c.upperLeft.x) }
    if (needed.isEmpty) Vector.empty
    else ranked.find(_._2 == needed).map(e => Vector(e._1)).getOrElse {
      val pair = ranked.indices.iterator.flatMap { i =>
        (i + 1 until ranked.size).iterator.filter(j => separate(ranked(i)._1, ranked(j)._1) &&
          ((ranked(i)._2 ++ ranked(j)._2) == needed)).map(j => Vector(ranked(i)._1, ranked(j)._1))
      }.take(1).toVector.headOption
      pair.getOrElse {
        var left = needed
        var selected = Vector.empty[Area]
        while (left.nonEmpty) {
          val next = ranked.filter(e => selected.forall(separate(e._1, _)))
            .map(e => (e._1, e._2 intersect left)).filter(_._2.nonEmpty)
            .sortBy(e => (-e._2.size, e._1.upperLeft.y, e._1.upperLeft.x)).headOption
          if (next.isEmpty) return Vector.empty
          selected :+= next.get._1
          left --= next.get._2
        }
        selected
      }
    }
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

class TerranBunkerDefense(universe: Universe)
  extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {
  private val builder = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val plans = mutable.Map.empty[Int, (Vector[MapPosition], Vector[Area])]
  private val garrison = new BunkerGarrison
  private var cargoObserved = Map.empty[Int, Set[Int]]
  private var announcedReady = Set.empty[Int]
  private var uncoveredReported = Set.empty[Int]
  private var placementReported = Set.empty[MapTilePosition]
  private def active = race.isTerran && strategy.current.isInstanceOf[Strategy.SimpleTerran]
  private def bunkers = ownUnits.allByType[Bunker].filter(_.isInGame).toVector
  private def nativeCargo(b: Bunker): Set[Int] = b.nativeUnit.getLoadedUnits.asScala.filter { u =>
    u.getType == bwapi.UnitType.Terran_Marine && u.isLoaded &&
      Option(u.getTransport).exists(_.getID == b.nativeUnitId)
  }.map(_.getID).toSet
  private val boarding = oncePerTick {
    if (active) {
      val ready = bunkers.filterNot(_.isBeingCreated)
      val slots = ready.map(b => (b.nativeUnitId, b.tilePosition, nativeCargo(b)))
      val candidates = ownUnits.allByType[Marine].filter(m => m.isInGame && !m.isBeingCreated &&
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
    if (!active || currentTick < 31 || currentTick % 31 != 0) return
    boarding.get
    val range = nativeGame.self().weaponMaxRange(bwapi.UnitType.Terran_Marine.groundWeapon()) + 64
    val fields = bases.allBases.filter(b => !b.mainBuilding.isBeingCreated && !b.mainBuilding.isFloating)
      .flatMap(_.resourceArea).groupBy(_.uniqueId).values.map(_.head).toVector
    fields.foreach { field =>
      if (!plans.contains(field.uniqueId)) {
        val workTiles = field.patches.toList.flatMap(_.patches.toVector.flatMap(_.area.growBy(1).tiles)).distinct
        val points = BunkerCoverage.corners(workTiles)
        val existing = bunkers.filter(b => b.tilePosition.distanceToIsLess(field.center, 15)).map(_.area)
        val candidates = new ConstructionSiteFinder(universe).bunkerSites(field)
        val sites = BunkerCoverage.select(points, candidates, existing, range)
        if (points.nonEmpty && (sites.nonEmpty || points.forall(p => existing.exists(BunkerCoverage.covers(_, p, range))))) {
          plans(field.uniqueId) = points -> (existing ++ sites)
          NativeMatchEvidence.trace("bunker-coverage-plan", s"field=${field.uniqueId} range=$range model=conservativeNativeApprox points=${points.size} sites=${(existing ++ sites).map(_.upperLeft)}")
          uncoveredReported -= field.uniqueId
        } else if (!uncoveredReported(field.uniqueId)) {
          NativeMatchEvidence.trace("bunker-coverage-unavailable", s"field=${field.uniqueId} points=${points.size} safeCandidates=${candidates.size} noGenericFallback=true")
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
                NativeMatchEvidence.trace("bunker-construction-request", s"field=${field.uniqueId} site=${site.upperLeft} nativeSpaceNow=$now error=${nativeGame.getLastError}")
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
    val desired = plans.values.map(_._2.size * 4).sum
    if (unitManager.countExistingAndPlanned(classOf[Marine]) < desired)
      requestUnit(classOf[Marine], takeCareOfDependencies = true)
    val cargo = bunkers.filterNot(_.isBeingCreated).map(b => b.nativeUnitId -> nativeCargo(b)).toMap
    if (currentTick % (31 * 16) == 0 && !coverageReady) {
      val nativeMarines = nativeGame.self().getUnits.asScala.filter(_.getType == bwapi.UnitType.Terran_Marine).toVector
      val eligible = ownUnits.allByType[Marine].filter(m => m.isInGame && !m.isBeingCreated).map(_.nativeUnitId).toVector
      val producers = ownUnits.allByType[Barracks].map(b => s"${b.nativeUnitId}:${b.nativeUnit.isTraining}:${b.nativeUnit.getRemainingTrainTime}:${unitManager.jobOf(b).shortDebugString}").toVector
      val assigned = bunkers.map(b => b.nativeUnitId -> boarding.get.reserved.toVector.filter(id => boarding.get.target(id).contains(b.nativeUnitId)))
      NativeMatchEvidence.trace("bunker-garrison-status", s"nativeMarines=${nativeMarines.map(_.getID)} eligible=$eligible planned=${unitManager.plannedToTrain.count(_.typeOfRequestedUnit == classOf[Marine])} producers=$producers assigned=$assigned")
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
