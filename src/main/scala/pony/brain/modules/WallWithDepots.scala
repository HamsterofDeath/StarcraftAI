package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

/** The wall opening seals the main base's land approach with Supply Depots. */
class WallWithDepots(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe)
    with BuildingRequestHelper {

  private var anchors             = Option.empty[Vector[MapTilePosition]]
  private var reportedNone        = false
  private var reportedComplete    = false
  private var refusedPlanning     = false
  private var probed              = false
  private var reportedSealFailure = false
  private var lastWallAttempt     = -1
  private var gateOpened          = false
  private var gateDepotIds        = Set.empty[Int]
  private val repairers           = new Employer[SCV](universe)
  private val demolishers         = new Employer[MobileRangeWeapon](universe)

  private def active = race.isTerran && strategy.current.usesWallDefense

  /** True once every planned wall depot stands completed. */
  def complete: Boolean = anchors.exists { wall =>
    wall.nonEmpty && wall.forall { a =>
      ownUnits.allByType[SupplyDepot].exists(d => d.isInGame && !d.isBeingCreated && d.tilePosition == a)
    }
  }

  /** True once planning concluded that no depot wall can seal the main approach. */
  def refused: Boolean = refusedPlanning

  /** True once the wall was deliberately opened so the army can leave the base. */
  def gateOpen: Boolean = gateOpened

  /** Knock down the depot (or pair) whose removal actually opens a walkable way out. */
  def openGate_!(): Unit = {
    if (!gateOpened) {
      gateOpened = true
      gateDepotIds = openDepots
      NativeMatchEvidence.trace(
        "wall-gate-open",
        s"depots=${if (gateDepotIds.isEmpty) "-" else gateDepotIds.mkString(",")}"
      )
    }
  }

  private def openDepots: Set[Int] = anchors.flatMap { wall =>
    bases.mainBase.flatMap { home =>
      def alive(a: MapTilePosition) = ownUnits.allByType[SupplyDepot]
        .find(d => d.isInGame && !d.isBeingCreated && d.tilePosition == a)
      val singles = wall.iterator.filter { a =>
        alive(a).isDefined && opensWay(home, footprint(a).toSet)
      }.take(1).map(a => Set(a)).toList
      val pairs = if (singles.nonEmpty) Nil
      else wall.combinations(2).filter { pair =>
        pair.forall(a => alive(a).isDefined) && opensWay(home, pair.flatMap(footprint).toSet)
      }.take(1).toList.map(_.toSet)
      (singles ++ pairs).headOption.map(_.flatMap(a => alive(a).map(_.nativeUnitId)))
    }
  }.getOrElse(Set.empty)

  /** True when freeing the given tiles lets a ground path from home escape the defended region. */
  private def opensWay(home: Base, freed: Set[MapTilePosition]): Boolean = {
    strategicMap.defenseLineOf(home).exists { front =>
      def allowed(t: MapTilePosition): Boolean =
        mapLayers.rawWalkableMap.insideBounds(t) && mapLayers.rawWalkableMap.free(t) &&
          (freed(t) ||
            (mapLayers.blockedByBuildingTiles.free(t) && mapLayers.blockedByPlannedBuildings.free(t))) &&
          mapLayers.blockedByResources.free(t)
      mapLayers.rawWalkableMap.nearestFree(home.mainBuilding.tilePosition).exists { start =>
        val visited = mutable.Set.empty[MapTilePosition]
        val queue   = mutable.Queue.empty[MapTilePosition]
        visited += start
        queue += start
        var escaped = false
        var steps   = 0
        while (queue.nonEmpty && !escaped && steps < 20000) {
          val cur = queue.dequeue()
          steps += 1
          if (!front.defended.free(cur)) escaped = true
          else for (dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0) {
            val n      = cur.movedBy(dx, dy)
            val corner = dx == 0 || dy == 0 || allowed(cur.movedBy(dx, 0)) || allowed(cur.movedBy(0, dy))
            if (!visited(n) && allowed(n) && corner) {
              visited += n
              queue += n
            }
          }
        }
        escaped
      }
    }
  }

  /** While the wall is incomplete, the economy's next supply depot should be a wall depot. */
  def nextSupplySpot: Option[MapTilePosition] = {
    if (refusedPlanning || gateOpened) None
    else anchors.flatMap { wall =>
      val existing = ownUnits.allByType[SupplyDepot].filter(_.isInGame).map(_.tilePosition).toSet
      val pending  =
        (unitManager.requestedConstructions[SupplyDepot].flatMap(_.customPosition.requestedPosition) ++
          unitManager.constructionsInProgress[SupplyDepot].map(_.buildWhere)).toSet
      wall.filterNot(a => existing(a) || pending(a)).find(depotFree)
    }
  }

  /** True while the given spot is still a planned, unoccupied and free wall anchor. */
  def supplySpotValid(spot: MapTilePosition): Boolean =
    anchors.exists(_.contains(spot)) && depotFree(spot)

  /** A 2x2 supply depot fits and is unoccupied: shared by the wall and the tank boxes. */
  def depotSpotFree(anchor: MapTilePosition): Boolean = depotFree(anchor)

  private def probe(reason: String): Unit = if (!probed) {
    probed = true
    NativeMatchEvidence.trace("wall-probe", reason)
  }

  private def depotFree(anchor: MapTilePosition): Boolean = {
    val area = Area(anchor, Size(2, 2))
    mapLayers.rawWalkableMap.insideBounds(anchor) && area.tiles.forall { t =>
      mapLayers.rawWalkableMap.free(t) &&
      mapLayers.freeTilesForConstruction.free(t) &&
      mapLayers.blockedByBuildingTiles.free(t) &&
      mapLayers.blockedByPlannedBuildings.free(t)
    }
  }

  private def placementsCovering(t: MapTilePosition): Vector[MapTilePosition] =
    Vector(t, MapTilePosition(t.x - 1, t.y), MapTilePosition(t.x, t.y - 1), MapTilePosition(t.x - 1, t.y - 1))
      .filter(depotFree)

  private def footprint(a: MapTilePosition) = Area(a, Size(2, 2)).tiles

  private def overlaps(a: MapTilePosition, b: MapTilePosition) = (a.x - b.x).abs < 2 && (a.y - b.y).abs < 2

  private def barrierForGround(t: MapTilePosition) =
    !mapLayers.rawWalkableMap.insideBounds(t) ||
      !mapLayers.rawWalkableMap.free(t) || mapLayers.blockedByResources.blocked(t)

  /** Walk from the end of the pass along the cut direction until a terrain or resource barrier. */
  private def extendToBarrier(from: MapTilePosition, dx: Int, dy: Int): Option[Vector[MapTilePosition]] = {
    val out = mutable.ArrayBuffer.empty[MapTilePosition]
    var t   = from.movedBy(dx, dy)
    while (out.size < 8 && !barrierForGround(t)) {
      out += t
      t = t.movedBy(dx, dy)
    }
    Option.when(barrierForGround(t))(out.toVector)
  }

  /** The pass the cut line crosses: the free run along the line around the choke center, pinned by
    * terrain or resource barriers on both sides (extending the line if it was truncated first). */
  private def segmentThrough(
      center: MapTilePosition,
      from: MapTilePosition,
      to: MapTilePosition
  ): Option[Vector[MapTilePosition]] = {
    val lineTiles = mutable.ArrayBuffer.empty[MapTilePosition]
    AreaHelper.traverseTilesOfLine(from, to, (x, y) => lineTiles += MapTilePosition(x, y))
    val freeIndices = lineTiles.indices.filter(i => !barrierForGround(lineTiles(i)))
    if (freeIndices.isEmpty) None
    else {
      val startIndex = freeIndices.minBy(i => lineTiles(i).distanceSquaredTo(center))
      var lo         = startIndex
      while (lo > 0 && !barrierForGround(lineTiles(lo - 1))) lo -= 1
      var hi = startIndex
      while (hi + 1 < lineTiles.size && !barrierForGround(lineTiles(hi + 1))) hi += 1
      val dx   = Integer.signum(to.x - from.x)
      val dy   = Integer.signum(to.y - from.y)
      val left = if (lo > 0) Some(Vector.empty[MapTilePosition])
      else extendToBarrier(lineTiles(lo), -dx, -dy)
      val right = if (hi < lineTiles.size - 1) Some(Vector.empty[MapTilePosition])
      else extendToBarrier(lineTiles(hi), dx, dy)
      for {
        l <- left
        r <- right
        segment = (l ++ lineTiles.slice(lo, hi + 1).toVector ++ r).distinct
        if segment.nonEmpty && segment.size <= 14
      } yield segment
    }
  }

  /** Span the pass with depots and check that no walkable path from outside to inside remains. */
  private def computeWall(home: Base): Vector[MapTilePosition] = {
    val front = strategicMap.defenseLineOf(home)
    if (front.isEmpty) { probe("no defense line"); return Vector.empty }
    val f        = front.get
    val segments = f.chokePoint.lines.flatMap(cutting =>
      segmentThrough(f.chokePoint.center, cutting.absoluteFrom, cutting.absoluteTo)
    )
    val span = segments.distinct.flatten.distinct
    if (span.isEmpty) { probe("no pinnable pass at the choke"); return Vector.empty }
    val allCandidates = span.flatMap(placementsCovering).distinct
    if (allCandidates.isEmpty) { probe(s"span=${span.size} candidates=0"); return Vector.empty }
    val coveredByCandidate = allCandidates.map(a => a -> footprint(a).filter(span.contains).toSet).toMap
    val tileToCandidates   = span.map(t => t -> allCandidates.filter(a => footprint(a).exists(_ == t))).toMap
    var budget             = 20000
    def solve(uncovered: Set[MapTilePosition], chosen: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] = {
      if (uncovered.isEmpty) Some(chosen)
      else if (chosen.size > 12 || budget <= 0) None
      else {
        budget -= 1
        val target = uncovered.minBy(t => (t.y, t.x))
        tileToCandidates.getOrElse(target, Vector.empty)
          .filterNot(a => chosen.exists(b => overlaps(a, b)))
          .sortBy(a => (-coveredByCandidate(a).count(uncovered.contains), a.y, a.x))
          .iterator.flatMap(a => solve(uncovered -- coveredByCandidate(a), chosen :+ a).iterator)
          .nextOption()
      }
    }
    val unbuildable = span.filter(t => tileToCandidates.getOrElse(t, Vector.empty).isEmpty)
    val required    = span.filterNot(unbuildable.contains)
    val solved      = solve(required.toSet, Vector.empty)
    if (solved.isEmpty) {
      probe(s"span=${span.size} candidates=${allCandidates.size} noCover missing=${unbuildable.mkString(",")}")
      return Vector.empty
    }
    if (unbuildable.nonEmpty) {
      NativeMatchEvidence.trace("wall-slit", s"unbuildable=${unbuildable.mkString(",")} depots=${solved.get.size}")
    }
    def covers(a: MapTilePosition) = footprint(a).filter(span.contains)

    // Protoss probes, zealots and dragoons must not slip between or around the depots.
    // A leak is plugged along the breach path, nearest to the wall first.
    var anchors  = solved.get
    var attempts = 0
    var breach   = breachPath(home, anchors)
    if (breach.isDefined) {
      NativeMatchEvidence.trace("wall-leak", s"pathLen=${breach.get.size} path=${breach.get.take(24).mkString(",")}")
    }
    while (breach.isDefined && attempts < 6 && anchors.size <= 20) {
      val path    = breach.get
      val pathSet = path.toSet
      val fix     = path.flatMap(placementsCovering).distinct
        .filterNot(anchors.contains)
        .filterNot(a => anchors.exists(b => overlaps(a, b)))
        .map { a =>
          val onPath       = footprint(a).count(t => pathSet.contains(t))
          val wallDistance = if (anchors.isEmpty) 0 else anchors.map(b => (a.x - b.x).abs.max((a.y - b.y).abs)).min
          val spanCover    = covers(a).size
          (a, onPath, wallDistance, spanCover)
        }
        .sortBy { case (a, onPath, wallDistance, spanCover) => (-onPath, wallDistance, -spanCover, a.y, a.x) }
        .headOption
      fix match {
        case Some((a, onPath, _, _)) =>
          anchors :+= a
          attempts += 1
          NativeMatchEvidence.trace("wall-seal-fix", s"attempt=$attempts at=$a onPath=$onPath")
        case None =>
          if (!reportedSealFailure) {
            reportedSealFailure = true
            NativeMatchEvidence.trace(
              "wall-seal-failed",
              s"pathLen=${path.size} buildable=${path.count(t => placementsCovering(t).nonEmpty)} anchors=${anchors.mkString(",")} path=${path.take(16).mkString(",")}"
            )
          }
          return Vector.empty
      }
      breach = breachPath(home, anchors)
    }
    if (breach.isDefined) {
      if (!reportedSealFailure) {
        reportedSealFailure = true
        NativeMatchEvidence.trace(
          "wall-seal-failed",
          s"attempts=$attempts pathLen=${breach.get.size} path=${breach.get.take(24).mkString(",")} anchors=${anchors.mkString(",")}"
        )
      }
      Vector.empty
    } else {
      probe(s"span=${span.size} depots=${anchors.size} sealed=true")
      anchors
    }
  }

  /** A free path from a known outside tile to the defended side means the wall leaks. */
  private def breachPath(home: Base, anchors: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] = {
    strategicMap.defenseLineOf(home).flatMap { front =>
      val wallTiles                            = anchors.flatMap(footprint).toSet
      def allowed(t: MapTilePosition): Boolean =
        mapLayers.rawWalkableMap.insideBounds(t) && mapLayers.rawWalkableMap.free(t) &&
          mapLayers.blockedByBuildingTiles.free(t) && mapLayers.blockedByPlannedBuildings.free(t) &&
          mapLayers.blockedByResources.free(t) &&
          !wallTiles(t)
      val seeds =
        (mapLayers.rawWalkableMap.spiralAround(front.chokePoint.center, 16) ++
          mapLayers.rawWalkableMap.spiralAround(front.chokePoint.center, 24))
          .filter(t => allowed(t) && front.outerTerritory.free(t) && !front.defended.free(t)).take(2)
      if (seeds.isEmpty) None
      else {
        val visited = mutable.Set.empty[MapTilePosition]
        val parent  = mutable.Map.empty[MapTilePosition, MapTilePosition]
        val queue   = mutable.Queue.empty[MapTilePosition]
        seeds.foreach { s => visited += s; queue += s }
        var breach = Option.empty[MapTilePosition]
        while (queue.nonEmpty && breach.isEmpty) {
          val cur = queue.dequeue()
          if (front.defended.free(cur)) breach = Some(cur)
          else for (dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0) {
            val n = cur.movedBy(dx, dy)
            // Units cannot cut a corner through two blocked tiles, so diagonal steps
            // are only legal when at least one shared orthogonal tile is walkable.
            val corner = dx == 0 || dy == 0 || allowed(cur.movedBy(dx, 0)) || allowed(cur.movedBy(0, dy))
            if (!visited(n) && allowed(n) && corner) {
              visited += n
              parent(n) = cur
              queue += n
            }
          }
        }
        breach.map { b =>
          val path = mutable.ArrayBuffer.empty[MapTilePosition]
          var p    = b
          path += p
          while (parent.contains(p)) { p = parent(p); path += p }
          path.toVector
        }
      }
    }
  }

  override def onTick_!(): Unit = {
    if (!active || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    bases.mainBase.foreach { home =>
      val wall = anchors.getOrElse {
        // A refused plan is retried rarely; terrain and the corridor do not change quickly.
        if (lastWallAttempt >= 0 && currentTick - lastWallAttempt < 24 * 60 * 5) Vector.empty
        else {
          lastWallAttempt = currentTick
          val chosen = computeWall(home)
          refusedPlanning = chosen.isEmpty
          if (chosen.nonEmpty) {
            anchors = Some(chosen)
            NativeMatchEvidence.trace("wall-planned", s"depots=${chosen.size} at=${chosen.mkString(",")}")
          }
          chosen
        }
      }
      if (wall.isEmpty) {
        if (refusedPlanning && !reportedNone) {
          NativeMatchEvidence.trace("wall-none", "no achievable land choke; skipping the depot wall")
          reportedNone = true
        }
      } else {
        // The economy's supply requests carry the wall anchors while any depot is missing;
        // see ProvideNewSupply, and WallWithDepots.nextSupplySpot for the spot it picks.
        val built = wall.forall { a =>
          ownUnits.allByType[SupplyDepot].exists(d => d.isInGame && !d.isBeingCreated && d.tilePosition == a)
        }
        if (built && !reportedComplete) {
          NativeMatchEvidence.trace("wall-complete", s"depots=${wall.size}")
          reportedComplete = true
        }
        // Repair the wall while it is attacked; the guard has no units yet in the opening.
        // Unfinished depots are included so the repair order resumes their construction.
        val damagedWall = wall.flatMap { a =>
          ownUnits.allByType[SupplyDepot].find(d =>
            d.isInGame && !d.isFloating && d.tilePosition == a
          )
        }.filter(d => d.nativeUnit.getHitPoints < d.nativeUnit.getType.maxHitPoints)
          .filterNot(d => gateOpened && gateDepotIds.contains(d.nativeUnitId))
        damagedWall.foreach { d =>
          val assigned = unitManager.allJobsByType[RepairWallDepot].count(j =>
            j.targetId == d.nativeUnitId && !j.failedOrObsolete && !j.isFinished
          )
          if (assigned < 2) {
            val nativeIds = nativeGame.self().getUnits.asScala.map(_.getID).toSet
            val request   = UnitJobRequest.idleOfType(repairers, classOf[SCV], 2 - assigned, Priority.Supply)
              .withOnlyAccepting { w =>
                val job    = unitManager.jobOf(w)
                val nearby = w.currentTile.distanceToIsLess(d.centerTile, 12) &&
                  w.currentArea.contains(d.areaOnMap)
                // An unfinished depot may be far from the base; any miner may resume it.
                nativeIds(w.nativeUnitId) && (d.isBeingCreated || nearby) &&
                (job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
              }.withRequest(_.withCherryPicker_!(
                UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](d.centerTile)()
              ))
            unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
              repairers.assignJob_!(new RepairWallDepot(w, d, repairers))
              NativeMatchEvidence.trace(
                "wall-repair-assigned",
                s"depot=${d.nativeUnitId} scv=${w.nativeUnitId} hp=${d.nativeUnit.getHitPoints}"
              )
            }
          }
        }
        if (gateOpened && gateDepotIds.nonEmpty) {
          ownUnits.allByType[SupplyDepot]
            .filter(d => d.isInGame && !d.isBeingCreated && gateDepotIds.contains(d.nativeUnitId))
            .foreach { depot =>
              val assigned = unitManager.allJobsByType[DemolishWallDepot]
                .count(j => j.targetId == depot.nativeUnitId && !j.failedOrObsolete && !j.isFinished)
              if (assigned < 3) makeDemolishers(depot, 3 - assigned)
            }
        }
      }
    }
  }

  private def makeDemolishers(depot: SupplyDepot, missing: Int): Unit = {
    def ask[T <: MobileRangeWeapon: ClassTag](cls: Class[T]): Unit = {
      val request = UnitJobRequest.idleOfType(demolishers, cls, missing, Priority.Supply)
        .withOnlyAccepting { w =>
          val job = unitManager.jobOf(w)
          job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch]
        }
      unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
        demolishers.assignJob_!(new DemolishWallDepot(w, depot, demolishers))
        NativeMatchEvidence.trace(
          "wall-gate-demolish",
          s"depot=${depot.nativeUnitId} unit=${w.nativeUnitId} hp=${depot.nativeUnit.getHitPoints}"
        )
      }
    }
    if (ownUnits.allByType[Tank].exists(t => t.isInGame && !t.isBeingCreated)) ask(classOf[Tank])
    else if (ownUnits.allByType[Vulture].exists(v => v.isInGame && !v.isBeingCreated)) ask(classOf[Vulture])
  }
}
