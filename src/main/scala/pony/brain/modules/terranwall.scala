package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** The wall opening seals the main base's land approach with Supply Depots. */
class WallWithDepots(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe)
  with BuildingRequestHelper {

  private var anchors = Option.empty[Vector[MapTilePosition]]
  private var reportedNone = false
  private var reportedComplete = false
  private var refusedPlanning = false
  private var probed = false
  private var reportedSealFailure = false
  private var lastWallAttempt = -1
  private val lastAttempt = mutable.Map.empty[MapTilePosition, Int]
  private val repairers = new Employer[SCV](universe)

  private def active = race.isTerran && (strategy.current match {
    case s: Strategy.SimpleTerran => s.usesWallDefense
    case _ => false
  })

  /** True once every planned wall depot stands completed. */
  def complete: Boolean = anchors.exists { wall =>
    wall.nonEmpty && wall.forall { a =>
      ownUnits.allByType[SupplyDepot].exists(d => d.isInGame && !d.isBeingCreated && d.tilePosition == a)
    }
  }

  /** True once planning concluded that no depot wall can seal the main approach. */
  def refused: Boolean = refusedPlanning

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

  /** Corridor tiles of the main choke. */
  private def corridorTiles(home: Base): Vector[MapTilePosition] = {
    val corridor = mutable.LinkedHashSet.empty[MapTilePosition]
    strategicMap.defenseLineOf(home).toVector.foreach { front =>
      front.chokePoint.lines.foreach { cutting =>
        AreaHelper.traverseTilesOfLine(cutting.absoluteFrom, cutting.absoluteTo,
          (x, y) => corridor += MapTilePosition(x, y))
      }
    }
    corridor.toVector
      .filter(mapLayers.rawWalkableMap.insideBounds)
      .filter(mapLayers.rawWalkableMap.free)
  }

  private def placementsCovering(t: MapTilePosition): Vector[MapTilePosition] =
    Vector(t, MapTilePosition(t.x - 1, t.y), MapTilePosition(t.x, t.y - 1), MapTilePosition(t.x - 1, t.y - 1))
      .filter(depotFree)

  private def footprint(a: MapTilePosition) = Area(a, Size(2, 2)).tiles

  private def overlaps(a: MapTilePosition, b: MapTilePosition) =
    (a.x - b.x).abs < 2 && (a.y - b.y).abs < 2

  /** Every corridor tile covered by a depot, and no walkable path from outside to inside remains. */
  private def computeWall(home: Base): Vector[MapTilePosition] = {
    val corridor = corridorTiles(home)
    if (corridor.isEmpty) { probe("corridor=0"); return Vector.empty }
    val candidates = corridor.flatMap(placementsCovering).distinct
    if (candidates.isEmpty) { probe(s"corridor=${corridor.size} candidates=0"); return Vector.empty }
    val covered = mutable.Set.empty[MapTilePosition]
    val chosen = mutable.ArrayBuffer.empty[MapTilePosition]
    def covers(a: MapTilePosition) = footprint(a).filter(corridor.contains)
    while (!corridor.forall(covered.contains) && chosen.size <= 8) {
      val best = candidates.filterNot(chosen.contains)
        .filterNot(a => chosen.exists(b => overlaps(a, b)))
        .maxByOpt(a => covers(a).count(t => !covered.contains(t)))
      best.filter(a => covers(a).exists(t => !covered.contains(t))) match {
        case Some(a) =>
          chosen += a
          covered ++= covers(a)
        case None =>
          probe(s"corridor=${corridor.size} candidates=${candidates.size} stuck uncovered=${corridor.count(t => !covered.contains(t))}")
          return Vector.empty
      }
    }
    if (!corridor.forall(covered.contains)) {
      probe(s"corridor=${corridor.size} candidates=${candidates.size} tooWide depots=${chosen.size} uncovered=${corridor.count(t => !covered.contains(t))}")
      return Vector.empty
    }

    // Protoss probes, zealots and dragoons must not slip between or around the depots.
    // A leak is plugged by extending the wall along the breach path, nearest to the wall first.
    var anchors = chosen.toVector
    var attempts = 0
    var breach = breachPath(home, anchors)
    while (breach.isDefined && attempts < 12 && anchors.size <= 16) {
      val path = breach.get
      val pathSet = path.toSet
      val fix = path.flatMap(placementsCovering).distinct
        .filterNot(anchors.contains)
        .filterNot(a => anchors.exists(b => overlaps(a, b)))
        .map { a =>
          val onPath = footprint(a).count(t => pathSet.contains(t))
          val wallDistance = anchors.map(b => (a.x - b.x).abs.max((a.y - b.y).abs)).min
          val corridorCover = covers(a).count(corridor.contains)
          (a, onPath, wallDistance, corridorCover)
        }
        .sortBy { case (a, onPath, wallDistance, corridorCover) => (-onPath, wallDistance, -corridorCover, a.y, a.x) }
        .headOption
      fix match {
        case Some((a, onPath, _, _)) =>
          anchors :+= a
          attempts += 1
          NativeMatchEvidence.trace("wall-seal-fix", s"attempt=$attempts at=$a onPath=$onPath")
        case None =>
          if (!reportedSealFailure) {
            reportedSealFailure = true
            NativeMatchEvidence.trace("wall-seal-failed",
              s"pathLen=${path.size} buildable=${path.count(t => placementsCovering(t).nonEmpty)} anchors=${anchors.mkString(",")} path=${path.take(16).mkString(",")}")
          }
          return Vector.empty
      }
      breach = breachPath(home, anchors)
    }
    if (breach.isDefined) {
      if (!reportedSealFailure) {
        reportedSealFailure = true
        NativeMatchEvidence.trace("wall-seal-failed", s"attempts=$attempts anchors=${anchors.mkString(",")}")
      }
      Vector.empty
    } else {
      probe(s"corridor=${corridor.size} depots=${anchors.size} sealed=true")
      anchors
    }
  }

  /** A free path from a known outside tile to the defended side means the wall leaks. */
  private def breachPath(home: Base, anchors: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] = {
    strategicMap.defenseLineOf(home).flatMap { front =>
      val wallTiles = anchors.flatMap(footprint).toSet
      def allowed(t: MapTilePosition): Boolean =
        mapLayers.rawWalkableMap.insideBounds(t) && mapLayers.rawWalkableMap.free(t) &&
          mapLayers.blockedByBuildingTiles.free(t) && mapLayers.blockedByPlannedBuildings.free(t) &&
          !wallTiles(t)
      val seeds = (mapLayers.rawWalkableMap.spiralAround(front.chokePoint.center, 16) ++
        mapLayers.rawWalkableMap.spiralAround(front.chokePoint.center, 24))
        .filter(t => allowed(t) && front.outerTerritory.free(t) && !front.defended.free(t)).take(2)
      if (seeds.isEmpty) None
      else {
        val visited = mutable.Set.empty[MapTilePosition]
        val parent = mutable.Map.empty[MapTilePosition, MapTilePosition]
        val queue = mutable.Queue.empty[MapTilePosition]
        seeds.foreach { s => visited += s; queue += s }
        var breach = Option.empty[MapTilePosition]
        while (queue.nonEmpty && breach.isEmpty) {
          val cur = queue.dequeue()
          if (front.defended.free(cur)) breach = Some(cur)
          else for (dx <- -1 to 1; dy <- -1 to 1 if dx != 0 || dy != 0) {
            val n = cur.movedBy(dx, dy)
            if (!visited(n) && allowed(n)) {
              visited += n
              parent(n) = cur
              queue += n
            }
          }
        }
        breach.map { b =>
          val path = mutable.ArrayBuffer.empty[MapTilePosition]
          var p = b
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
        val existing = ownUnits.allByType[SupplyDepot].filter(_.isInGame).map(_.tilePosition).toSet
        val pending = (unitManager.requestedConstructions[SupplyDepot].flatMap(_.customPosition.requestedPosition) ++
          unitManager.constructionsInProgress[SupplyDepot].map(_.buildWhere)).toSet
        val missing = wall.filterNot(a => existing(a) || pending(a))
        if (missing.isEmpty) {
          if (!reportedComplete) {
            NativeMatchEvidence.trace("wall-complete", s"depots=${wall.size}")
            reportedComplete = true
          }
        } else {
          missing.filter(depotFree).foreach { a =>
            val last = lastAttempt.getOrElse(a, -1)
            val due = last < 0 || currentTick - last > 24 * 60
            if (due && !pending(a) && !existing(a)) {
              lastAttempt(a) = currentTick
              NativeMatchEvidence.trace("wall-depot-request", s"at=$a")
              requestBuilding(classOf[SupplyDepot], takeCareOfDependencies = false,
                customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(a)(depotFree(a)),
                priority = Priority.Expand)
            }
          }
        }
        // Repair the wall while it is attacked; the guard has no units yet in the opening.
        val damagedWall = wall.flatMap { a =>
          ownUnits.allByType[SupplyDepot].find(d =>
            d.isInGame && !d.isBeingCreated && !d.isFloating && d.tilePosition == a)
        }.filter(d => d.nativeUnit.getHitPoints < d.nativeUnit.getType.maxHitPoints)
        damagedWall.foreach { d =>
          val assigned = unitManager.allJobsByType[RepairWallDepot].count(j =>
            j.targetId == d.nativeUnitId && !j.failedOrObsolete && !j.isFinished)
          if (assigned < 2) {
            val nativeIds = nativeGame.self().getUnits.asScala.map(_.getID).toSet
            val request = UnitJobRequest.idleOfType(repairers, classOf[SCV], 2 - assigned, Priority.Supply)
              .withOnlyAccepting { w =>
                val job = unitManager.jobOf(w)
                nativeIds(w.nativeUnitId) && w.currentTile.distanceToIsLess(d.centerTile, 12) &&
                  w.currentArea.contains(d.areaOnMap) &&
                  (job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
              }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](d.centerTile)()))
            unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
              repairers.assignJob_!(new RepairWallDepot(w, d, repairers))
              NativeMatchEvidence.trace("wall-repair-assigned", s"depot=${d.nativeUnitId} scv=${w.nativeUnitId} hp=${d.nativeUnit.getHitPoints}")
            }
          }
        }
      }
    }
  }
}

private[pony] object WallRepairState {
  sealed trait State
  case object Repairing extends State
  case object Finished extends State
  case object Failed extends State
  def apply(workerAlive: Boolean, targetAlive: Boolean, damaged: Boolean, floating: Boolean): State =
    if (!workerAlive) Failed else if (!targetAlive || !damaged) Finished else if (floating) Failed else Repairing
}

/** An SCV patches a wall depot while it is under attack and returns to mining afterwards. */
private[pony] class RepairWallDepot(worker: SCV, depot: SupplyDepot, owner: Employer[SCV])
  extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString = s"Repair wall depot ${depot.nativeUnitId}"
  private def state = WallRepairState(worker.nativeUnit.exists && !worker.isDead, depot.nativeUnit.exists,
    depot.nativeUnit.getHitPoints < depot.nativeUnit.getType.maxHitPoints, depot.isFloating)
  override def isFinished = state == WallRepairState.Finished
  override def jobHasFailedWithoutDeath = state == WallRepairState.Failed
  override def everyNth = 23
  override def ordersForTick = Orders.RepairBuilding(worker, depot).toSeq
  def targetId = depot.nativeUnitId
}
