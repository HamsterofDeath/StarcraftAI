package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

/**
  * The wall opening seals the main base's land approach with Supply Depots and, where the geometry allows, a Barracks
  * as its gate: the barracks lifts to let the army or a scout through and lands again to close the wall.
  */
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
  private var gate                = Option.empty[MapTilePosition]
  private var gateBarracksId      = Option.empty[Int]
  private var gateWasOpen         = false
  private var gateRequested       = false
  private var gateChangedAt       = 0
  private val demolishers         = new Employer[MobileRangeWeapon](universe)

  private def active = race.isTerran && strategy.current.usesWallDefense

  /** True once every planned wall depot and the gate barracks stand completed. */
  def complete: Boolean = anchors.exists { wall =>
    (wall.nonEmpty || gate.isDefined) && wall.forall { a =>
      ownUnits.allByType[SupplyDepot].exists(d => d.isInGame && !d.isBeingCreated && d.tilePosition == a)
    }
  } && gate.forall(_ => gateBarracks.exists(!_.isBeingCreated))

  /** The barracks planned as the wall's gate, landed in its slot or lifted out of it. */
  private def gateBarracks: Option[Barracks] =
    gateBarracksId.flatMap(id => ownUnits.allByType[Barracks].find(b => b.nativeUnitId == id && b.isInGame)).orElse {
      gate.flatMap(g => ownUnits.allByType[Barracks].find(b => b.isInGame && !b.isFloating && b.tilePosition == g))
        .map { b => gateBarracksId = Some(b.nativeUnitId); b }
    }

  /** True once planning concluded that no depot wall can seal the main approach. */
  def refused: Boolean = refusedPlanning

  /** True for a wall depot the army is knocking down to open the gate: nobody repairs it. */
  def demolishing(id: Int): Boolean = gateOpened && gateDepotIds.contains(id)

  /** True once the wall was deliberately opened, or while its barracks gate is lifted. */
  def gateOpen: Boolean = gateOpened || gateBarracks.exists(_.isFloating)

  /** Knock down the depot (or pair) whose removal actually opens a walkable way out; a barracks gate lifts instead. */
  def openGate_!(): Unit = {
    if (!gateOpened && gate.isEmpty) {
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

  private def depotFree(anchor: MapTilePosition): Boolean = footprintFree(Area(anchor, Size(3, 2)))

  private def barracksFree(anchor: MapTilePosition): Boolean = footprintFree(Area(anchor, Size(4, 3)))

  private def footprintFree(area: Area): Boolean = {
    mapLayers.rawWalkableMap.insideBounds(area.upperLeft) && mapLayers.rawWalkableMap.insideBounds(area.lowerRight) &&
    area.tiles.forall { t =>
      mapLayers.rawWalkableMap.free(t) &&
      mapLayers.freeTilesForConstruction.free(t) &&
      mapLayers.blockedByBuildingTiles.free(t) &&
      mapLayers.blockedByPlannedBuildings.free(t)
    }
  }

  // A supply depot covers 3x2 tiles.
  private def placementsCovering(t: MapTilePosition): Vector[MapTilePosition] =
    (for (dx <- 0 to 2; dy <- 0 to 1) yield MapTilePosition(t.x - dx, t.y - dy)).toVector.filter(depotFree)

  private def footprint(a: MapTilePosition) = Area(a, Size(3, 2)).tiles

  private def overlaps(a: MapTilePosition, b: MapTilePosition) = (a.x - b.x).abs < 3 && (a.y - b.y).abs < 2

  private def barracksTiles(b: MapTilePosition) = Area(b, Size(4, 3)).tiles

  private def barracksCovering(t: MapTilePosition): Vector[MapTilePosition] =
    (for (dx <- 0 to 3; dy <- 0 to 2) yield MapTilePosition(t.x - dx, t.y - dy)).toVector.filter(barracksFree)

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
    // `taken` tiles belong to the gate barracks; no depot may overlap them
    def solve(
        uncovered: Set[MapTilePosition],
        chosen: Vector[MapTilePosition],
        taken: Set[MapTilePosition] = Set.empty
    ): Option[Vector[MapTilePosition]] = {
      if (uncovered.isEmpty) Some(chosen)
      else if (chosen.size > 12 || budget <= 0) None
      else {
        budget -= 1
        val target = uncovered.minBy(t => (t.y, t.x))
        tileToCandidates.getOrElse(target, Vector.empty)
          .filterNot(a => chosen.exists(b => overlaps(a, b)) || footprint(a).exists(taken))
          .sortBy(a => (-coveredByCandidate(a).count(uncovered.contains), a.y, a.x))
          .iterator.flatMap(a => solve(uncovered -- coveredByCandidate(a), chosen :+ a, taken).iterator)
          .nextOption()
      }
    }
    val unbuildable = span.filter(t => tileToCandidates.getOrElse(t, Vector.empty).isEmpty)
    val required    = span.filterNot(unbuildable.contains)

    // Prefer a barracks as the gate, on the choke line or a parallel line up to two tiles in or out: a ramp's width
    // varies, and a barracks (three tiles) and depots (two) must fill the pass exactly.
    val (linePlan, lineChecks) = gatePlan(home, f.chokePoint)
    val (anyPlan, freeCheck)   = linePlan.fold(freeGatePlan(home, f.chokePoint))(p => (Some(p), "free:skipped"))
    anyPlan match {
      case Some((depots, barracks)) =>
        gate = Some(barracks)
        NativeMatchEvidence.trace(
          "wall-gate-planned",
          s"barracks=$barracks depots=${depots.mkString(",")} line=${linePlan.isDefined}"
        )
        return depots
      case None =>
        NativeMatchEvidence.trace(
          "wall-gate-none",
          s"checks=${lineChecks.mkString(" ")} $freeCheck; depots only, opened by demolition"
        )
    }
    budget = 20000
    val solved = solve(required.toSet, Vector.empty)
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

  /** Depots (3x2) covering every tile of `required` without overlapping each other or the `taken` tiles. */
  private def coverWithDepots(
      required: Set[MapTilePosition],
      taken: Set[MapTilePosition]
  ): Option[Vector[MapTilePosition]] = {
    val candidates = required.toVector.flatMap(placementsCovering).distinct.filterNot(a => footprint(a).exists(taken))
    val covers     = candidates.map(a => a -> footprint(a).filter(required).toSet).toMap
    val byTile     = required.map(t => t -> candidates.filter(a => covers(a)(t))).toMap
    var budget     = 20000
    def solve(uncovered: Set[MapTilePosition], chosen: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] =
      if (uncovered.isEmpty) Some(chosen)
      else if (chosen.size > 8 || budget <= 0) None
      else {
        budget -= 1
        val target = uncovered.minBy(t => (t.y, t.x))
        byTile.getOrElse(target, Vector.empty)
          .filterNot(a => chosen.exists(b => overlaps(a, b)))
          .sortBy(a => (-covers(a).count(uncovered.contains), a.y, a.x))
          .iterator.flatMap(a => solve(uncovered -- covers(a), chosen :+ a).iterator)
          .nextOption()
      }
    solve(required, Vector.empty)
  }

  /**
    * A barracks gate and the depots that close the rest of the pass, on the choke line or a parallel line shifted up to
    * two tiles: accepted only if a zealot cannot pass with the barracks landed and a siege tank can pass with it lifted
    * (judged on pixels). Also returns one diagnosis per line.
    */
  private def gatePlan(
      home: Base,
      choke: ChokePoint
  ): (Option[(Vector[MapTilePosition], MapTilePosition)], Vector[String]) = {
    val checks = mutable.ArrayBuffer.empty[String]
    val plans  = for {
      shift   <- Iterator(0, 1, -1, 2, -2)
      cutting <- choke.lines.iterator
      (dx, dy) = (
        Integer.signum(cutting.absoluteTo.x - cutting.absoluteFrom.x),
        Integer.signum(cutting.absoluteTo.y - cutting.absoluteFrom.y)
      )
      (nx, ny) = (-dy * shift, dx * shift)
      span <- segmentThrough(
        choke.center.movedBy(nx, ny),
        cutting.absoluteFrom.movedBy(nx, ny),
        cutting.absoluteTo.movedBy(nx, ny)
      ).iterator
    } yield {
      val spots = span.flatMap(barracksCovering).distinct
        .sortBy(b =>
          (-barracksTiles(b).count(span.contains), b.movedBy(2, 1).distanceSquaredTo(choke.center), b.y, b.x)
        )
        .take(8)
      var covered = 0
      val plan    = spots.iterator.flatMap { b =>
        val taken = barracksTiles(b).toSet
        coverWithDepots(span.filterNot(taken).toSet, taken).iterator.flatMap { depots =>
          covered += 1
          val closes = pixelPass(home, WallGeometry.Dims.Zealot, depots, Some(b)).contains(false)
          val opens  = closes && pixelPass(home, WallGeometry.Dims.SiegeTank, depots, None).contains(true)
          Option.when(closes && opens)(depots -> b)
        }
      }.nextOption()
      checks += s"shift=$shift:span=${span.size}:spots=${spots.size}:covered=$covered:ok=${plan.isDefined}"
      plan
    }
    val found = plans.flatten.nextOption()
    (found, checks.toVector)
  }

  /**
    * Whether `unit` gets from outside the main's choke to the defended side past the planned depots and the gate
    * barracks (when landed), judged on pixels against building boxes and walk-tile terrain within nine tiles of the
    * choke; None without a known outside start.
    */
  private def pixelPass(
      home: Base,
      unit: WallGeometry.Dims,
      depots: Vector[MapTilePosition],
      barracks: Option[MapTilePosition]
  ): Option[Boolean] = pixelWay(home, unit, depots, barracks).map(_.isDefined)

  /** The way `pixelPass` finds (Some(None) when there is none); None without a known outside start. */
  private def pixelWay(
      home: Base,
      unit: WallGeometry.Dims,
      depots: Vector[MapTilePosition],
      barracks: Option[MapTilePosition]
  ): Option[Option[Vector[(Int, Int)]]] = strategicMap.defenseLineOf(home).flatMap { front =>
    import WallGeometry._
    val grid     = mapLayers.rawWalkableMap
    val c        = front.chokePoint.center
    val (x0, y0) = ((c.x - 9).max(0), (c.y - 9).max(0))
    val (x1, y1) = ((c.x + 9).min(grid.cols - 1), (c.y + 9).min(grid.rows - 1))
    val region   = Box(x0 * 32, y0 * 32, (x1 + 1) * 32 - 1, (y1 + 1) * 32 - 1)
    val planned  = depots.map(a => buildingBox(a.x, a.y, Dims.SupplyDepot)) ++
      barracks.map(b => buildingBox(b.x, b.y, Dims.Barracks))
    val standing = (nativeGame.getAllUnits.asScala ++ nativeGame.getStaticNeutralUnits.asScala).iterator
      .filter(u =>
        (u.getType.isBuilding || u.getType.isMineralField || u.getType.isRefinery ||
          u.getType == bwapi.UnitType.Resource_Vespene_Geyser) && !u.isFlying
      )
      .map(u => Box(u.getLeft, u.getTop, u.getRight, u.getBottom)).filter(_.intersects(region)).toVector
    def blocked(wx: Int, wy: Int) =
      wx < 0 || wy < 0 || wx >= grid.cols * 4 || wy >= grid.rows * 4 || !nativeGame.isWalkable(wx, wy)
    val starts = (grid.spiralAround(c, 16) ++ grid.spiralAround(c, 24))
      .filter(t => grid.free(t) && front.outerTerritory.free(t) && !front.defended.free(t))
      .filter(t => t.x >= x0 && t.x <= x1 && t.y >= y0 && t.y <= y1).distinct.take(6)
      .map(t => (t.x * 32 + 16, t.y * 32 + 16)).toVector
    Option.when(starts.nonEmpty)(
      path(
        unit,
        planned ++ standing,
        blocked,
        region,
        starts,
        (x, y) => front.defended.free(MapTilePosition(x / 32, y / 32))
      )
    )
  }

  /**
    * A gate wall built on pixels rather than along one line: for barracks spots near the choke, depots are added where
    * a zealot's way through runs, nearest to the choke first, until no way is left (at most four depots); the plan is
    * kept if a siege tank gets through once the barracks lifts.
    */
  private def freeGatePlan(
      home: Base,
      choke: ChokePoint
  ): (Option[(Vector[MapTilePosition], MapTilePosition)], String) = {
    import WallGeometry.Dims
    val c     = choke.center
    val spots = (for (dx <- -7 to 4; dy <- -6 to 4) yield c.movedBy(dx, dy)).filter(barracksFree)
      .sortBy(b => (b.movedBy(2, 1).distanceSquaredTo(c), b.y, b.x)).take(10)
    var tried = 0
    val plan  = spots.iterator.flatMap { b =>
      tried += 1
      val taken  = barracksTiles(b).toSet
      var depots = Vector.empty[MapTilePosition]
      var way    = pixelWay(home, Dims.Zealot, depots, Some(b))
      var stuck  = way.isEmpty
      while (way.exists(_.isDefined) && depots.size < 4 && !stuck) {
        val tiles = way.get.get.map((x, y) => MapTilePosition(x / 32, y / 32)).distinct
        val onWay = tiles.toSet
        tiles.flatMap(placementsCovering).distinct
          .filterNot(a => footprint(a).exists(taken) || depots.exists(d => overlaps(a, d)))
          .sortBy(a => (a.movedBy(1, 0).distanceSquaredTo(c), -footprint(a).count(onWay), a.y, a.x))
          .headOption match {
          case Some(a) =>
            depots :+= a
            way = pixelWay(home, Dims.Zealot, depots, Some(b))
          case None => stuck = true
        }
      }
      val closed = way.contains(None) && !stuck
      Option.when(closed && pixelPass(home, Dims.SiegeTank, depots, None).contains(true))(depots -> b)
    }.nextOption()
    (plan, s"free:spots=${spots.size}:tried=$tried:ok=${plan.isDefined}")
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
    if (!active) return
    // every module tick: the gate must close quickly when enemies come, and building requests live only briefly
    requestGate()
    controlGate()
    if (currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    bases.mainBase.foreach { home =>
      val wall = anchors.getOrElse {
        // A refused plan is retried rarely; terrain and the corridor do not change quickly.
        if (lastWallAttempt >= 0 && currentTick - lastWallAttempt < 24 * 60 * 5) Vector.empty
        else {
          lastWallAttempt = currentTick
          val chosen = computeWall(home)
          refusedPlanning = chosen.isEmpty && gate.isEmpty
          if (chosen.nonEmpty || gate.isDefined) {
            anchors = Some(chosen)
            NativeMatchEvidence.trace(
              "wall-planned",
              s"depots=${chosen.size} at=${chosen.mkString(",")} gate=$gate home=${home.mainBuilding.tilePosition} " +
                s"geysirs=${home.myGeysirs.map(_.tilePosition).mkString(",")}"
            )
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

  /** Renews the request for the gate barracks until it stands (requests expire unless repeated). */
  private def requestGate(): Unit = gate.foreach { g =>
    val pending = unitManager.requestedConstructions[Barracks].exists(_.customPosition.requestedPosition.contains(g)) ||
      unitManager.constructionsInProgress[Barracks].exists(_.buildWhere == g)
    if (gateBarracks.isEmpty && !pending && barracksFree(g)) {
      requestBuilding(classOf[Barracks], customBuildingPosition = AlternativeBuildingSpot.fromPreset(g))
      if (!gateRequested) NativeMatchEvidence.trace("wall-gate-request", s"barracks=$g")
      gateRequested = true
    }
  }

  /**
    * The barracks gate opens (lifts) while the army is out, attacking or reinforcing, or scouting is allowed, and closes
    * (lands in its slot) whenever enemy ground fighters come within ten tiles of it. A barracks still training cancels
    * its queue before it lifts.
    */
  private def controlGate(): Unit = for (g <- gate; b <- gateBarracks if !b.isBeingCreated) {
    val campaign = universe.pluginByType[RunTerranCampaign]
    val armyOut  = worldDominationPlan.campaignForceSize > 0
    val wantOpen = armyOut || campaign.reconnaissanceAllowed || campaign.minimalScoutingActive
    val danger   = unitGrid.enemy.allInRange[Mobile](g.movedBy(2, 1), 10)
      .exists(e => !e.nativeUnit.isFlying && !e.isInstanceOf[WorkerUnit])
    // hysteresis: the barracks takes seconds to lift or land, so an open gate stays open 20 seconds unless enemies
    // come, and a closed one stays closed 5 seconds
    val held = currentTick - gateChangedAt
    val open =
      if (danger) false
      else if (gateWasOpen) wantOpen || held < 24 * 20
      else wantOpen && held >= 24 * 5
    val native = b.nativeUnit
    if (open && !b.isFloating) {
      if (native.isTraining) native.cancelTrain() else native.lift()
    } else if (!open && b.isFloating && native.getOrder != bwapi.Order.BuildingLand) native.land(g.asTilePosition)
    if (open != gateWasOpen) {
      gateWasOpen = open
      gateChangedAt = currentTick
      NativeMatchEvidence.trace(
        "wall-gate",
        s"${if (open) "open" else "close"} army=$armyOut danger=$danger barracks=${b.nativeUnitId}"
      )
    }
  }

  private def makeDemolishers(depot: SupplyDepot, missing: Int): Unit = {
    def ask[T <: MobileRangeWeapon: ClassTag](cls: Class[T]): Unit = {
      val request = UnitJobRequest.idleOfType(demolishers, cls, missing, Priority.Supply)
        .withOnlyAccepting { w =>
          val job = unitManager.jobOf(w)
          // fighters are never idle: their default behaviours employ them, and those may be taken
          job.isIdle || job.isInstanceOf[BusyDoingSomething[?]] || job.isInstanceOf[GatherMineralsAtSinglePatch]
        }
      unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
        demolishers.assignJob_!(new DemolishWallDepot(w, depot, demolishers))
        NativeMatchEvidence.trace(
          "wall-gate-demolish",
          s"depot=${depot.nativeUnitId} unit=${w.nativeUnitId} hp=${depot.nativeUnit.getHitPoints}"
        )
      }
    }
    // tanks and vultures knock a depot down fastest; without them any ranged ground fighter does, or the gate never
    // opens and the base stays sealed
    def present[T <: WrapsUnit: ClassTag] =
      ownUnits.allByType[T].exists(u => u.isInGame && !u.nativeUnit.isFlying && !u.isInstanceOf[WorkerUnit])
    if (present[Tank]) ask(classOf[Tank])
    else if (present[Vulture]) ask(classOf[Vulture])
    else if (present[Goliath]) ask(classOf[Goliath])
    else if (present[Marine]) ask(classOf[Marine])
  }
}
