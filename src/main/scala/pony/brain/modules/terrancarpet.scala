package pony
package brain
package modules

import scala.collection.mutable

/** Carpet knobs; every value is overridable with a -Dtwailight.carpet... system property. */
private[pony] object CarpetQuotas {
  private def intProp(name: String, defaultValue: Int) =
    sys.props.get(name).flatMap(_.toIntOption).filter(_ > 0).getOrElse(defaultValue)
  def tanksPerPost: Int = intProp("twailight.carpetTanksPerPost", 2)
  def vulturesPerPost: Int = intProp("twailight.carpetVulturesPerPost", 1)
  def goliathsPerPost: Int = intProp("twailight.carpetGoliathsPerPost", 1)
  def tanksBeforeFlight: Int = intProp("twailight.carpetFlyTanks", 2)
  def homeGuards: Int = intProp("twailight.carpetHomeGuards", 6)
  /** Zero spreads over every resource area on the map. */
  def maxPosts: Int = sys.props.get("twailight.carpetPosts").flatMap(_.toIntOption).filter(_ > 0).getOrElse(0)
  /** Depot boxes around carpet tanks; switch off with -Dtwailight.carpetBoxes=0. */
  def tankBoxesEnabled: Boolean = sys.props.get("twailight.carpetBoxes").forall(_ != "0")
  /** Only spend on boxes once this many minerals are unlocked. */
  def tankBoxMinMinerals: Int = intProp("twailight.carpetBoxMinMinerals", 600)
  def tankBoxMaxDepots: Int = intProp("twailight.carpetBoxMaxDepots", 8)
  /** Open the wall once this many fighters exist and the second base stands. */
  def gateFighters: Int = intProp("twailight.carpetGateFighters", 12)
}

/** Pure farthest-point ordering so the carpet posts spread evenly over the map. */
private[pony] object CarpetPosts {
  def order(candidates: Vector[MapTilePosition], count: Int): Vector[MapTilePosition] = {
    val distinct = candidates.distinct
    if (distinct.isEmpty || count <= 0) Vector.empty
    else {
      val first = distinct.minBy(t => (t.y, t.x))
      val chosen = mutable.ArrayBuffer(first)
      val remaining = mutable.Set.empty[MapTilePosition] ++ distinct
      remaining.remove(first)
      while (chosen.size < count && remaining.nonEmpty) {
        val next = remaining.maxBy { t =>
          val distances = chosen.iterator.map(c => t.distanceSquaredTo(c)).toVector
          (distances.min, distances.sum, -t.y, -t.x)
        }
        chosen += next
        remaining.remove(next)
      }
      chosen.toVector
    }
  }
}

/** Spreads tanks, vultures and goliaths evenly over the map instead of one death ball. */
class CarpetSpread(universe: Universe) extends OrderlessAIModule[Mobile](universe) {
  private val assignments = mutable.Map.empty[Int, MapTilePosition]
  private var reportedPosts = false

  def postOf(unitId: Int): Option[MapTilePosition] = assignments.get(unitId)

  private def carpet = strategy.current.isInstanceOf[Strategy.TerranCarpet]
  private def wall = universe.pluginByType[WallWithDepots]

  private val pocket = oncePerTick {
    bases.mainBase.flatMap(home => strategicMap.defenseLineOf(home.mainBuilding.tilePosition))
  }

  /** A sealed wall turns the home plateau into a pocket ground units cannot leave. */
  private def insidePocket(t: MapTilePosition) = wall.complete && pocket.get.exists(_.defended.free(t))

  private val plannedPosts = oncePerTick {
    val home = bases.mainBase.flatMap(_.resourceArea).map(_.uniqueId)
    val candidates = strategicMap.resources.filterNot(a => home.contains(a.uniqueId))
      .map(_.nearbyFreeTile).toVector.filter(mapLayers.rawWalkableMap.insideBounds)
    val count = if (CarpetQuotas.maxPosts > 0) CarpetQuotas.maxPosts else candidates.size
    CarpetPosts.order(candidates, count)
  }

  private def kind(m: Mobile) = m match {
    case _: Tank => 0
    case _: Vulture => 1
    case _ => 2
  }

  private def quota(kind: Int) = kind match {
    case 0 => CarpetQuotas.tanksPerPost
    case 1 => CarpetQuotas.vulturesPerPost
    case _ => CarpetQuotas.goliathsPerPost
  }

  override def onTick_!(): Unit = {
    if (!carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    // The wall protects the opening; once a real army stands on a second base it becomes the
    // defense, and one depot is demolished so the army can reach its map-wide posts.
    if (wall.complete && !wall.gateOpen) {
      val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
      val army = ownUnits.allMobilesWithWeapons.count(m => m.isInGame && !m.isBeingCreated &&
        m.isFigher && !m.isInstanceOf[WorkerUnit])
      if (operational && army >= CarpetQuotas.gateFighters) wall.openGate_!()
    }
    val posts = plannedPosts.get
    if (posts.nonEmpty && !reportedPosts) {
      NativeMatchEvidence.trace("carpet-posts", s"posts=${posts.size} at=${posts.mkString(",")}")
      reportedPosts = true
    }
    val units = ownUnits.allMobilesWithWeapons.filter(m => m.isInGame && !m.isBeingCreated &&
      (m.isInstanceOf[Tank] || m.isInstanceOf[Vulture] || m.isInstanceOf[Goliath]))
      .groupBy(_.nativeUnitId).values.map(_.head).toVector.sortBy(_.nativeUnitId)
    assignments.retain((id, post) => posts.contains(post) && units.exists(_.nativeUnitId == id))
    val counts = mutable.Map.empty[(MapTilePosition, Int), Int]
    assignments.foreach { case (id, post) =>
      units.find(_.nativeUnitId == id).foreach(u => counts((post, kind(u))) = counts.getOrElse((post, kind(u)), 0) + 1)
    }
    val home = bases.mainBase.map(_.mainBuilding.tilePosition)
    val homeQuota = if (wall.complete) 0 else CarpetQuotas.homeGuards
    val eligible = units.filterNot(u => assignments.contains(u.nativeUnitId))
      .sortBy(u => (home.map(h => u.currentTile.distanceSquaredTo(h)).getOrElse(0), u.nativeUnitId))
      .filterNot(u => insidePocket(u.currentTile))
      .drop(homeQuota)
    eligible.foreach { u =>
      val k = kind(u)
      posts.minByOpt { p =>
        val existing = counts.getOrElse((p, k), 0)
        (if (existing >= quota(k)) 1 else 0, existing, u.currentTile.distanceSquaredTo(p))
      }.foreach { p =>
        assignments(u.nativeUnitId) = p
        counts((p, k)) = counts.getOrElse((p, k), 0) + 1
        NativeMatchEvidence.trace("carpet-assign", s"unit=${u.nativeUnitId} post=$p")
      }
    }
    if (currentTick % (31 * 16) == 0) {
      NativeMatchEvidence.trace("strategy-carpet",
        s"posts=${posts.size} units=${units.size} assigned=${assignments.size} homeGuards=$homeQuota wallComplete=${wall.complete} wallRefused=${wall.refused}")
    }
  }
}

/** With plenty of minerals, seal each stationary carpet tank behind a ring of depots. */
class TankBoxes(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe)
  with BuildingRequestHelper {
  private val done = mutable.Set.empty[Int]
  private var activeBox = Option.empty[(Int, Vector[MapTilePosition])]

  private def carpet = strategy.current.isInstanceOf[Strategy.TerranCarpet]
  private def wall = universe.pluginByType[WallWithDepots]
  private def spread = universe.pluginByType[CarpetSpread]

  private def coveredAnchors = {
    val existing = ownUnits.allByType[SupplyDepot].filter(_.isInGame).map(_.tilePosition).toSet
    val pending = (unitManager.requestedConstructions[SupplyDepot].flatMap(_.customPosition.requestedPosition) ++
      unitManager.constructionsInProgress[SupplyDepot].map(_.buildWhere)).toSet
    existing ++ pending
  }

  /** The tiles a melee unit could use to attack the tank, relative to its 2x2 footprint. */
  private def ringOf(t: MapTilePosition): Vector[MapTilePosition] =
    (for (dx <- -1 to 2; dy <- -1 to 2 if dx < 0 || dx > 1 || dy < 0 || dy > 1)
      yield MapTilePosition(t.x + dx, t.y + dy)).toVector

  private def walkableFree(t: MapTilePosition) =
    mapLayers.rawWalkableMap.insideBounds(t) && mapLayers.rawWalkableMap.free(t) &&
      mapLayers.blockedByBuildingTiles.free(t) && mapLayers.blockedByPlannedBuildings.free(t) &&
      mapLayers.blockedByResources.free(t)

  private def overlapsTank(anchor: MapTilePosition, tank: MapTilePosition) =
    anchor.x <= tank.x + 1 && tank.x <= anchor.x + 1 && anchor.y <= tank.y + 1 && tank.y <= anchor.y + 1

  /** Depot anchors that block every walkable attack tile; terrain provides the rest. */
  private def boxPlan(tank: Tank): Option[Vector[MapTilePosition]] = {
    val t = tank.currentTile
    val needCover = ringOf(t).filter(walkableFree).distinct
    if (needCover.isEmpty) None
    else {
      val anchors = (for (dx <- -2 to 2; dy <- -2 to 2) yield MapTilePosition(t.x + dx, t.y + dy)).toVector
        .filterNot(a => overlapsTank(a, t)).filter(wall.depotSpotFree)
      val footprint = anchors.map(a => a -> Area(a, Size(2, 2)).tiles.toVector).toMap
      val covers = anchors.filter(a => footprint(a).exists(needCover.contains))
      val coveredBy = covers.map(a => a -> footprint(a).filter(needCover.contains).toSet).toMap
      val tileToCandidates = needCover.map(r => r -> covers.filter(a => coveredBy(a).contains(r))).toMap
      val maxDepots = math.min(CarpetQuotas.tankBoxMaxDepots, 12)
      var budget = 20000
      def solve(uncovered: Set[MapTilePosition], chosen: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] = {
        if (uncovered.isEmpty) Some(chosen)
        else if (chosen.size >= maxDepots || budget <= 0) None
        else {
          budget -= 1
          val target = uncovered.minBy(r => (r.y, r.x))
          tileToCandidates.getOrElse(target, Vector.empty)
            .filterNot(a => chosen.exists(b => (a.x - b.x).abs < 2 && (a.y - b.y).abs < 2))
            .sortBy(a => (-coveredBy(a).count(uncovered.contains), a.y, a.x))
            .iterator.flatMap(a => solve(uncovered -- coveredBy(a), chosen :+ a).iterator)
            .nextOption()
        }
      }
      solve(needCover.toSet, Vector.empty)
    }
  }

  override def onTick_!(): Unit = {
    if (!CarpetQuotas.tankBoxesEnabled || !carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    if (!(wall.complete || wall.refused || wall.gateOpen)) return
    activeBox match {
      case Some((tankId, anchors)) =>
        val missing = anchors.filterNot(coveredAnchors)
        if (missing.isEmpty) {
          NativeMatchEvidence.trace("tank-box-complete", s"tank=$tankId depots=${anchors.size}")
          done += tankId
          activeBox = None
        } else if (!ownUnits.allByType[Tank].exists(t => t.isInGame && t.nativeUnitId == tankId)) {
          activeBox = None
        } else {
          missing.find(wall.depotSpotFree).foreach { a =>
            requestBuilding(classOf[SupplyDepot], takeCareOfDependencies = false,
              customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(a)(wall.depotSpotFree(a)),
              priority = Priority.Expand)
            NativeMatchEvidence.trace("tank-box-depot", s"tank=$tankId at=$a")
          }
        }
      case None =>
        if (universe.resources.unlockedResources.minerals < CarpetQuotas.tankBoxMinMinerals) return
        val candidate = ownUnits.allByType[Tank]
          .filter(t => t.isInGame && !t.isBeingCreated && !t.nativeUnit.isMoving && !done(t.nativeUnitId))
          .toVector.sortBy(_.nativeUnitId)
          .iterator.flatMap { tank =>
            spread.postOf(tank.nativeUnitId)
              .filter(p => tank.currentTile.distanceToIsLess(p, 8))
              .flatMap(_ => boxPlan(tank).map(anchors => tank.nativeUnitId -> anchors))
          }.take(1).toList.headOption
        candidate.foreach { case (id, anchors) =>
          activeBox = Some((id, anchors))
          NativeMatchEvidence.trace("tank-box-plan", s"tank=$id depots=${anchors.size} at=${anchors.mkString(",")}")
        }
    }
  }
}

/** Once the wall stands and tanks exist, factories fly out to the open fields and produce there. */
class FlyFactoriesToNatural(universe: Universe) extends OrderlessAIModule[Factory](universe) {
  private val employers = new Employer[Factory](universe)
  private var flight = Option.empty[RelocateFactory]
  private val claimedFields = collection.mutable.Set.empty[Int]

  private def carpet = strategy.current.isInstanceOf[Strategy.TerranCarpet]
  private def wall = universe.pluginByType[WallWithDepots]

  override def onTick_!(): Unit = {
    if (!carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    flight = flight.filterNot(j => j.failedOrObsolete || j.isFinished)
    if (flight.isDefined) return
    val wallSealed = wall.complete || wall.refused || wall.gateOpen
    val tanks = ownUnits.allByType[Tank].count(t => t.isInGame && !t.isBeingCreated)
    if (!wallSealed || tanks < CarpetQuotas.tanksBeforeFlight) return
    val homeAreaId = bases.mainBase.map(_.mainBuilding.tilePosition)
    ownUnits.allByType[Factory].filter(f => f.isInGame && !f.isBeingCreated && !f.isFloating &&
      !f.nativeUnit.isTraining && f.nativeUnit.getRemainingTrainTime == 0 &&
      homeAreaId.forall(ht => f.tilePosition.distanceToIsLess(ht, 20)) &&
      unitManager.jobOf(f).isIdle)
      .toVector.sortBy(f => (f.tilePosition.y, f.tilePosition.x, f.nativeUnitId))
      .headOption.foreach { factory =>
        val request = UnitJobRequest.idleOfType(employers, classOf[Factory], priority = Priority.Expand)
          .withOnlyAccepting(_.nativeUnitId == factory.nativeUnitId)
        unitManager.request(request).units.headOption.foreach { unit =>
          val job = new RelocateFactory(employers, unit, claimedFields)
          employers.assignJob_!(job)
          flight = Some(job)
          NativeMatchEvidence.trace("factory-flight",
            s"id=${unit.nativeUnitId} from=${unit.tilePosition} tanks=$tanks wallComplete=${wall.complete}")
        }
      }
  }
}

/** Lift one factory, fly it to the open field nearest home and land it there. */
private[pony] class RelocateFactory(employer: Employer[Factory], factory: Factory,
                                    claimedFields: collection.mutable.Set[Int])
  extends UnitWithJob[Factory](employer, factory, Priority.Expand) {
  private var destination = Option.empty[MapTilePosition]
  private var destinationField = Option.empty[ResourceArea]
  private var phase = Option.empty[DepotRelocation.Step]
  private var lastTile = factory.tilePosition
  private var lastProgress = currentTick
  private var landingRetries = 0
  private var failedFields = Set.empty[Int]
  private var lastTelemetry = 0

  private def home = bases.mainBase.map(_.mainBuilding.tilePosition)

  private def safe(area: ResourceArea) =
    mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile) &&
      unitGrid.enemy.allInRange[Mobile](area.nearbyFreeTile, 12).isEmpty

  private def chooseDestination(): Unit = {
    val homeTile = home.getOrElse(factory.tilePosition)
    val homeAreaId = bases.mainBase.flatMap(_.resourceArea).map(_.uniqueId)
    val forceHome = factory.isFloating && failedFields.size >= 2
    val found = if (forceHome) None else strategicMap.resources.filterNot(a => homeAreaId.contains(a.uniqueId))
      .filterNot(a => failedFields.contains(a.uniqueId) || claimedFields.contains(a.uniqueId))
      .filter(safe)
      .toVector.sortBy(a => (a.nearbyFreeTile.distanceSquaredTo(homeTile), a.uniqueId))
      .iterator.flatMap { area =>
        new ConstructionSiteFinder(universe).findSpotFor(area.nearbyFreeTile, classOf[Factory], maxRange = 35)
          .map(area -> _)
      }.take(1).toList.headOption
    destination = found.map(_._2)
    destinationField = found.map(_._1)
    found.foreach { case (area, _) => claimedFields += area.uniqueId }
    if (destination.isEmpty && factory.isFloating) {
      // Never strand a lifted factory: fall back to the home plateau.
      destination = new ConstructionSiteFinder(universe).findSpotFor(homeTile, classOf[Factory], maxRange = 25)
      destinationField = None
    }
  }

  override def everyNth = 31
  override def shortDebugString = "Relocate factory: " + phase
  override def isFinished = phase.contains(DepotRelocation.Established)
  override def jobHasFailedWithoutDeath = false
  override def ordersForTick: Seq[UnitOrder] = {
    if (destination.isEmpty) chooseDestination()
    destination.toList.flatMap { landingTile =>
      val tile = factory.tilePosition
      if (tile != lastTile) { lastTile = tile; lastProgress = currentTick }
      val landedThere = !factory.isFloating && factory.nativeUnit.isCompleted && tile == landingTile
      val next = DepotRelocation.next(true, false, factory.isFloating,
        tile.distanceToIsLess(landingTile, 4), landedThere)
      if (!phase.contains(next)) NativeMatchEvidence.trace("factory-flight",
        s"id=${factory.nativeUnitId} phase=$next from=$tile to=$landingTile field=${destinationField.map(_.uniqueId).getOrElse(-1)}")
      phase = Some(next)
      next match {
        case DepotRelocation.Lift =>
          if (factory.nativeUnit.canLift()) Orders.LiftBuilding(factory).toSeq else Nil
        case DepotRelocation.Fly =>
          if (currentTick - lastTelemetry > 240) {
            lastTelemetry = currentTick
            NativeMatchEvidence.trace("factory-flight",
              s"id=${factory.nativeUnitId} phase=Fly at=${factory.tilePosition} moving=${factory.nativeUnit.isMoving} to=$landingTile sinceProgress=${currentTick - lastProgress}")
          }
          val stuck = currentTick - lastProgress > 24 * 120
          if (stuck && factory.isFloating && factory.nativeUnit.canLand(factory.tilePosition.asTilePosition)) {
            // Cannot reach the field; settle where we are instead of hovering forever.
            destination = Some(factory.tilePosition)
            destinationField = None
            lastProgress = currentTick
            Nil
          } else if (destinationField.exists(f => !safe(f)) || stuck) {
            destinationField.foreach(f => failedFields += f.uniqueId)
            destination = None
            destinationField = None
            lastProgress = currentTick
            Nil
          } else if (!factory.isFloating) {
            // Landed somewhere unexpected mid-flight; lift again and keep going.
            if (factory.nativeUnit.canLift()) Orders.LiftBuilding(factory).toSeq else Nil
          } else if (factory.nativeUnit.isMoving) Nil
          else {
            // Long flights can stall at the edge of what the engine can path for a building;
            // hop toward the target so every order makes bounded progress.
            val here = factory.tilePosition
            val distance = math.max(math.abs(landingTile.x - here.x), math.abs(landingTile.y - here.y))
            val step = math.min(8, distance)
            val towards = MapTilePosition(here.x + Integer.signum(landingTile.x - here.x) * step,
              here.y + Integer.signum(landingTile.y - here.y) * step)
            Orders.FlyBuilding(factory, towards).toSeq
          }
        case DepotRelocation.Land =>
          if (factory.nativeUnit.canLand(landingTile.asTilePosition)) {
            landingRetries = 0
            Orders.LandBuilding(factory, landingTile).toSeq
          } else {
            landingRetries += 1
            if (landingRetries >= 8) {
              // Units can temporarily occupy the footprint. Search nearby instead of stranding a lifted factory.
              val alternatives = mapLayers.rawWalkableMap.spiralAround(landingTile, 8)
              alternatives.find(p => factory.nativeUnit.canLand(p.asTilePosition)).foreach { alternate =>
                destination = Some(alternate)
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
