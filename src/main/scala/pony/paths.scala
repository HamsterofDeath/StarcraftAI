package pony

import pony.astar.{AStarSearch, GridNode2DInt, Heuristics}
import pony.brain.modules._
import pony.brain.{Base, HasUniverse, Universe}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class MigrationPath(follow: Paths, override val universe: Universe) extends HasUniverse {
  def finalDestination = follow.unsafeTarget
  def safeDestination = follow.requestedTargetSafe
  private val remaining       = mutable.HashMap.empty[Mobile, ArrayBuffer[MapTilePosition]]
  private val counter         = mutable.HashMap.empty[Mobile, Int]
  private val helper          = new FormationHelper(universe, follow, 2)
  private val atFormationStep = mutable.HashSet.empty[Mobile]

  def targetFormationTiles = helper.formationTiles

  def allReachedDestination = remaining.nonEmpty &&
                              remaining.iterator
                              .filter(_._1.isInGame)
                              .forall(_._2.isEmpty)

  def nextFor(t: Mobile) = {
    val index = counter.getOrElseUpdate(t, counter.size) % follow.pathCount
    val initialFullPath = follow.paths(index)
    val todo = remaining.getOrElseUpdate(t, ArrayBuffer.empty ++= initialFullPath.waypoints)
    val closest = todo.minByOpt(_.distanceSquaredTo(t.currentTile))
    val formationPoint = helper.assignedPosition(t)
    val canSeeFormationPoint = formationPoint.exists { target =>
      target.distanceToIsLess(t.currentTile, 15) &&
      mapLayers.rawWalkableMap.connectedByLine(target, t.currentTile)
    }
    def isOver30Percent = todo.size / initialFullPath.waypoints.size.toDouble <= 0.7
    def manyArrived = atFormationStep.size.toDouble / counter.size >= 0.8

    if (manyArrived) {
      todo.clear()
      Some(follow.unsafeTarget -> true)
    } else if (canSeeFormationPoint) {
      todo.clear()
      atFormationStep += t
      formationPoint.map(e => e -> isOver30Percent)
    } else {
      closest.map { c =>
        if (c.distanceSquaredTo(t.currentTile) <= 25) {
          todo.removeUntilInclusive(_ == c)
        }
        c -> isOver30Percent
      }
    }
  }
}

case class Path(waypoints: Seq[MapTilePosition], solved: Boolean, solvable: Boolean,
                bestEffort: MapTilePosition, requestedTarget: MapTilePosition,
                unsafeTarget: MapTilePosition) {
  lazy val length = {
    if (waypoints.size <= 1)
      0
    else
      waypoints.sliding(2, 1).map { case Seq(a, b) => a.distanceTo(b) }.sum
  }
  def isPerfectSolution = solved
}

case class Paths(paths: Seq[Path]) {
  def isEmpty = paths.forall(_.waypoints.isEmpty)

  def minimalDistanceTo(tile: MapTilePosition) = {
    if (paths.isEmpty)
      0.0
    else
      math.sqrt(paths.iterator.flatMap(_.waypoints.map(_.distanceSquaredTo(tile))).min)
  }

  val pathCount           = paths.size
  val requestedTargetSafe = paths.headOption.map(_.requestedTarget).get
  val unsafeTarget        = paths.headOption.map(_.unsafeTarget).get
  val realisticTarget     = MapTilePosition.average(paths.map(_.bestEffort))
  val anyTarget           = paths.head.bestEffort
}

object PathFinder {

  implicit def convBack(gn: GridNode2DInt): MapTilePosition = MapTilePosition
                                                              .shared(gn.getX, gn.getY)

  def on(map: Grid2D) = {
    new PathFinder(map)
  }

  def to2DArray(grid: Grid2D) = {

    val asGrid = Array.ofDim[GridNode2DInt](grid.cols, grid.rows)
    implicit def conv(mtp: MapTilePosition): GridNode2DInt = {
      val ret = asGrid(mtp.x)(mtp.y)
      assert(ret != null, s"$mtp was null?")
      ret
    }
    // fill the grid
    grid.allFree.foreach { e =>
      asGrid(e.x)(e.y) = new GridNode2DInt(e.x, e.y) {
        override def suggestHeuristics(): Heuristics[GridNode2DInt] = {
          (from: GridNode2DInt, to: GridNode2DInt) => from.distanceTo(to).toInt
        }
        override def supportsShortcuts(): Boolean = true
        override def canReachDirectly(node: GridNode2DInt) = {
          grid.connectedByLine(e, node)
        }
      }
    }
    // connect the grid
    grid.allFree.foreach { mtp =>
      val (left, right, up, down) = mtp.leftRightUpDown
      val leftFree = grid.containsAndFree(left)
      val rightFree = grid.containsAndFree(right)
      val upFree = grid.containsAndFree(up)
      val downFree = grid.containsAndFree(down)
      val here = asGrid(mtp.x)(mtp.y)
      if (leftFree) here.addNode(left)
      if (rightFree) here.addNode(right)
      if (upFree) here.addNode(up)
      if (downFree) here.addNode(down)
      if (leftFree && upFree) {
        val moved = mtp.movedBy(-1, -1)
        if (grid.free(moved)) {
          here.addNode(moved)
        }
      }
      if (leftFree && downFree) {
        val moved = mtp.movedBy(-1, 1)
        if (grid.free(moved)) {
          here.addNode(moved)
        }
      }
      if (rightFree && upFree) {
        val moved = mtp.movedBy(1, -1)
        if (grid.free(moved)) {
          here.addNode(moved)
        }
      }
      if (rightFree && downFree) {
        val moved = mtp.movedBy(1, 1)
        if (grid.free(moved)) {
          here.addNode(moved)
        }
      }
    }

    asGrid
  }

}

class PathFinder(on: Grid2D) {

  import PathFinder._

  def this(mapLayers: MapLayers, safe: Boolean) {
    this {
      if (safe) {
        mapLayers.freeWalkableMapSafe.guaranteeImmutability
      } else {
        mapLayers.freeWalkableIgnoringMobiles.guaranteeImmutability
      }
    }
  }

  def findPaths(from: MapTilePosition, to: MapTilePosition, paths: Int = 10,
                tryFixPath: Boolean = true) = BWFuture {
    val fromFixed = {if (tryFixPath) on.nearestFree(from) else Some(from)}
    val toFixed = {
      if (tryFixPath) {
        fromFixed.flatMap { e =>
          on.areaWhichContainsAsFree(e).flatMap(_.nearestFree(to))
        }
      } else {Some(to)}
    }
    warn(s"Could not fix start $from", fromFixed.isEmpty)
    warn(s"Could not fix goal $to", toFixed.isEmpty)
    val unsafeTarget = to
    for (a <- fromFixed; b <- toFixed) yield spawn.findPaths(a, b, paths, unsafeTarget)
  }

  def findPathNow(from: MapTilePosition, to: MapTilePosition,
                  tryFixPath: Boolean = true): Option[Path] = {
    findPaths(from, to, 1, tryFixPath).blockAndGet.flatMap(_.paths.headOption)
  }

  def spawn = new AstarPathFinder(to2DArray(on))

  class AstarPathFinder(grid: Array[Array[GridNode2DInt]]) {

    def basedOn = on

    def findPathBestEffort(from: MapTilePosition, to: MapTilePosition,
                           unsafeTarget: MapTilePosition) = {
      val fromSafe = basedOn.nearestFree(from).getOr(s"Sorry, could not find alternative for $from")
      val toSafe = basedOn.areaWhichContainsAsFree(fromSafe).flatMap(_.nearestFree(to))
                   .getOr(s"Sorry, could not find alternative for $to")
      findPath(fromSafe, toSafe, unsafeTarget)
    }

    def findPath(from: MapTilePosition, to: MapTilePosition, unsafeTarget: MapTilePosition) = {
      val a = new AStarSearch[GridNode2DInt](from, to).performSearch()
      Path(a.getSolution.asScala
           .map(e => MapTilePosition.shared(e.getX, e.getY))
           .toVector,
        a.isSolved, !a.isUnsolvable, a.getTargetOrNearestReachable, to, unsafeTarget)
    }

    def findPaths(from: MapTilePosition, to: MapTilePosition, width: Int,
                  unsafeTarget: MapTilePosition) = {
      debug(s"Searching path from $from to $to", tick > 1)
      val finder = new AStarSearch[GridNode2DInt](from, to)
      var first = Option.empty[Path]
      def pathFrom(seq: Seq[MapTilePosition]) = {
        Path(seq, finder.isSolved, !finder.isUnsolvable, finder.getTargetOrNearestReachable, to,
          unsafeTarget)
      }
      val paths = (0 to width).iterator.map { _ =>
        finder.performSearch()
        val waypoints = finder.getFullSolution
                        .asScala
                        .map(e => e: MapTilePosition)
                        .sliding(1, 4)
                        .flatten
                        .toVector
        //block the path, then search again to get streets
        finder.getFullSolution.asScala.drop(15).dropRight(10).foreach(_.remove())
        if (first.isEmpty) first = Some(pathFrom(waypoints))
        waypoints
      }.takeWhile { candidate =>
        candidate.forall { pointOnLine =>
          first.get.waypoints.exists { old =>
            on.connectedByLine(old, pointOnLine)
          }
        }
      }.toVector
      Paths(paths.map(pathFrom))
    }
    private implicit def conv(mtp: MapTilePosition): GridNode2DInt = {
      val ret = grid(mtp.x)(mtp.y)
      assert(ret != null, s"Map tile $mtp is not free!")
      ret
    }
  }
}

class AreaHelper(source: Grid2D) {
  private val baseOn = source.ensureContainsBlocked

  def findFreeAreas = {
    val areas = mutable.ArrayBuffer.empty[Grid2D]
    val used = mutable.BitSet.empty
    val knownInAnyArea = new MutableGrid2D(baseOn.cols, baseOn.rows, used)
    baseOn.allFree.foreach { p =>
      if (knownInAnyArea.free(p)) {
        val area = floodFill(p)
        if (area.nonEmpty) {
          areas += {
            val asGrid = new Grid2D(baseOn.cols, baseOn.rows, area, false)
            used |= mutable.BitSet.fromBitMaskNoCopy(area.toBitMask)
            asGrid
          }
        }
      }
    }
    val ret = areas.toSeq.sortBy(-_.freeCount)
    assert({
      val before = baseOn.freeCount
      val after = ret.map(_.freeCount)
      before == after.sum
    }, {
      val merged = new MutableGrid2D(baseOn.cols, baseOn.rows, mutable.BitSet.empty, false)
      ret.foreach { grid =>
        grid.allFree.foreach { p =>
          assert(merged.blocked(p), s"$p should be blocked, but is free")
          merged.free_!(p)
          assert(merged.free(p))
        }
      }
      s"""
         |Sum of single areas is not equal to the complete area:
         |${baseOn.zoomedOut.mkString}
         |vs
         |${merged.zoomedOut.mkString}
         """.stripMargin
    })
    ret.sortBy(_.freeCount).reverse
  }

  private def floodFill(start: MapTilePosition) = {
    val ret = mutable.BitSet.empty
    AreaHelper.traverseTilesOfArea(start, (x, y) => ret += x + y * baseOn.cols, baseOn)
    ret.immutableWrapper
  }
  def directLineOfSight(a: Area, b: Area): Boolean = {
    b.outline.exists(p => directLineOfSight(a, p))
  }
  def directLineOfSight(a: Area, b: MapTilePosition): Boolean = {
    a.outline.exists(p => directLineOfSight(p, b))
  }
  def directLineOfSight(a: MapTilePosition, b: MapTilePosition): Boolean = {
    AreaHelper.directLineOfSight(a, b, baseOn)
  }
}

object AreaHelper {

  def directLineOfSight(a: MapTilePosition, b: MapTilePosition, grid2D: Grid2D): Boolean = {
    AreaHelper.traverseTilesOfLine(a, b, (x, y) => {
      if (grid2D.blocked(x, y)) Some(false) else None
    }, true)
  }
  def traverseTilesOfLine[T](a: MapTilePosition, b: MapTilePosition, f: (Int, Int) => Option[T],
                             orElse: T): T = {
    var startX = a.x
    var startY = a.y
    val endX = b.x
    val endY = b.y
    var dy: Int = endY - startY
    var dx: Int = endX - startX
    var stepY: Int = 0
    if (dy < 0) {
      dy = -dy
      stepY = -1
    }
    else {
      stepY = 1
    }
    var stepX: Int = 0
    if (dx < 0) {
      dx = -dx
      stepX = -1
    }
    else {
      stepX = 1
    }
    dy <<= 1
    dx <<= 1
    f(startX, startY).foreach { e => return e }
    if (dx > dy) {
      var fraction: Int = dy - (dx >> 1)
      while (startX != endX) {
        if (fraction >= 0) {
          startY += stepY
          fraction -= dx
        }
        startX += stepX
        fraction += dy
        f(startX, startY).foreach { e => return e }
      }
    }
    else {
      var fraction: Int = dx - (dy >> 1)
      while (startY != endY) {
        if (fraction >= 0) {
          startX += stepX
          fraction -= dy
        }
        startY += stepY
        fraction += dx
        f(startX, startY).foreach { e => return e }
      }
    }
    orElse
  }
  def traverseTilesOfLine[T](a: MapTilePosition, b: MapTilePosition, f: (Int, Int) => T): Unit = {
    traverseTilesOfLine(a, b, (x, y) => {f(x, y); None}, None)
  }
  def freeAreaSize(start: MapTilePosition, baseOn: Grid2D) = {
    var count = 0
    traverseTilesOfArea(start, (x, y) => {
      count += 1
    }, baseOn)
    count
  }

  def traverseTilesOfArea(start: MapTilePosition, f: (Int, Int) => Unit, baseOn: Grid2D): Unit = {
    val sizeX = baseOn.cols
    val sizeY = baseOn.rows
    if (baseOn.free(start)) {
      val taken = mutable.HashSet.empty[MapTilePosition]
      val open = mutable.ListBuffer.empty[MapTilePosition]
      open += start
      taken += start

      while (open.nonEmpty) {
        val head = open.head
        open.remove(0)
        f(head.x, head.y)
        if (head.x > 0) {
          val left = head.movedBy(-1, 0)
          if (!taken(left) && baseOn.free(left)) {
            open += left
            taken += left
          }
        }
        if (head.y > 0) {
          val up = head.movedBy(0, -1)
          if (!taken(up) && baseOn.free(up)) {
            open += up
            taken += up
          }
        }
        if (head.x < sizeX - 1) {
          val right = head.movedBy(1, 0)
          if (!taken(right) && baseOn.free(right)) {
            open += right
            taken += right
          }
        }
        if (head.y < sizeY - 1) {
          val down = head.movedBy(0, 1)
          if (!taken(down) && baseOn.free(down)) {
            open += down
            taken += down
          }
        }

      }
    }
  }

}

class ViewOnGrid(ug: UnitGrid, hostile: Boolean) {
  def onTile(tile: MapTilePosition) = ug.onTile(tile, hostile)
  def allInRange[T <: Mobile : Manifest](tile: MapTilePosition, radius: Int) = ug.allInRangeOf[T](
    tile, radius,
    !hostile)
}

class UnitGrid(override val universe: Universe) extends HasUniverse {
  val own   = new ViewOnGrid(this, false)
  val enemy = new ViewOnGrid(this, true)
  private val map        = universe.world.map
  private val myUnits    = Array.ofDim[mutable.HashSet[Mobile]](map.tileSizeX, map.tileSizeY)
  private val enemyUnits = Array.ofDim[mutable.HashSet[Mobile]](map.tileSizeX, map.tileSizeY)
  private val touched    = mutable.HashSet.empty[mutable.HashSet[Mobile]]
  def onTile(tile: MapTilePosition, hostile: Boolean) = {
    val set = on(hostile)(tile.x)(tile.y)
    if (set != null) set else Set.empty[Mobile]
  }
  private def on(hostile: Boolean) = if (hostile) enemyUnits else myUnits
  override def onTick(): Unit = {
    super.onTick()
    //reset
    touched.foreach { modified =>
      modified.clear()
    }
    touched.clear()
    // update
    universe.ownUnits.allCompletedMobiles.foreach { m =>
      val units = {
        val pos = m.currentTile
        val existing = myUnits(pos.x)(pos.y)
        if (existing == null) {
          val newSet = mutable.HashSet.empty[Mobile]
          myUnits(pos.x)(pos.y) = newSet
          newSet
        } else {
          existing
        }
      }
      touched += units
      units += m
    }
    universe.enemyUnits.allCompletedMobiles.foreach { m =>
      val units = {
        val pos = m.currentTile
        val existing = enemyUnits(pos.x)(pos.y)
        if (existing == null) {
          val newSet = mutable.HashSet.empty[Mobile]
          enemyUnits(pos.x)(pos.y) = newSet
          newSet
        } else {
          existing
        }
      }
      touched += units
      units += m
    }

  }
  def allInRangeOf[T <: Mobile : Manifest](position: MapTilePosition, radius: Int,
                                           friendly: Boolean): Traversable[T] = {
    val onWhat = on(!friendly)

    geoHelper
    val fromX = 0 max position.x - radius
    val toX = map.tileSizeX min position.x + radius
    val fromY = 0 max position.y - radius
    val toY = map.tileSizeY min position.y + radius
    val radSqr = radius * radius
    val x2 = position.x
    val y2 = position.y
    def dstSqr(x: Int, y: Int) = {
      val xx = x - x2
      val yy = y - y2
      xx * xx + yy * yy
    }

    new Traversable[T] {
      override def foreach[U](f: (T) => U): Unit = {
        val filter = manifest[T].runtimeClass
        for (x <- fromX until toX; y <- fromY until toY
             if dstSqr(x, y) <= radSqr) {
          val mobiles = onWhat(x)(y)
          if (mobiles != null) {
            val byType = mobiles.iterator.filter(filter.isInstance)
            byType.foreach { e =>
              f(e.asInstanceOf[T])
            }
          }
        }
      }
    }
  }
}

class MapLayers(override val universe: Universe) extends HasUniverse {

  private val rawMapWalk                 = world.map.walkableGrid
  private val empty                      = world.map.walkableGrid.emptySameSize(false)
                                           .guaranteeImmutability
  private val rawMapWalk2                = world.map.walkableGrid.mutableCopy
  private val rawMapBuild                = world.map.buildableGrid.mutableCopy
  private val plannedBuildings           = world.map.empty.zoomedOut.mutableCopy
  private var justBuildings              = evalOnlyBuildings
  private val justAreasToDefend          = evalOnlyAreasToDefend
  private var justMines                  = evalOnlyMines
  private val justBlockedForMainBuilding = evalOnlyBlockedForMainBuildings
  private var justMineralsAndGas         = evalOnlyResources
  private var justWorkerPaths            = evalWorkerPaths
  private var justBlockingMobiles        = evalOnlyMobileBlockingUnits
  private var justAddonLocations         = evalPotentialAddonLocations

  private var withBuildings                   = evalWithBuildings
  private var withBuildingsAndResources       = evalWithBuildingsAndResources
  private var withEverythingStaticBuildable   = evalEverythingStaticBuildable
  private var withEverythingStaticWalkable    = evalEverythingStaticWalkable
  private var withEverythingBlockingBuildable = evalEverythingBlockingBuildable
  private var withEverythingBlockingWalkable  = evalEverythingBlockingWalkable
  private val freeWalkableAndSafe             = evalSafe
  private val coveredCloaked                  = evalDetected
  private val coveredGround                   = evalGroundDefended
  private val coveredAir                      = evalAirDefended

  private var lastUpdatePerformedInTick       = universe.currentTick

  def isOnIsland(tilePosition: MapTilePosition) = {
    val areaInQuestion = rawMapWalk.areas.find(_.free(tilePosition))
    val maxArea = rawWalkableMap.areas.sortBy(-_.freeCount)
    if (maxArea.size > 1) {
      val main = maxArea.head
      val second = maxArea(1)
      val assumeIslandMap = second.freeCount * 2 > main.freeCount
      assumeIslandMap || !areaInQuestion.contains(main)
    } else false

  }
  def rawWalkableMap = rawMapWalk

  def defendedTiles = {
    update()
    justAreasToDefend.mostRecent.getOrElse(empty).guaranteeImmutability
  }
  def blockedByPotentialAddons = {
    update()
    justAddonLocations.asReadOnlyView
  }
  def blockedByPlannedBuildings = plannedBuildings.asReadOnlyView

  def freeWalkableMapSafe = freeWalkableAndSafe.mostRecent.getOrElse(rawMapWalk)

  def freeTilesForConstruction = {
    update()
    withEverythingStaticBuildable.asReadOnlyView
  }
  def buildableBlockedByNothingTiles = {
    update()
    withEverythingBlockingBuildable.asReadOnlyView
  }
  def freeWalkableTiles = {
    update()
    withEverythingBlockingWalkable.asReadOnlyView
  }
  def freeWalkableIgnoringMobiles = {
    update()
    withEverythingStaticWalkable.asReadOnlyView
  }
  def blockedByBuildingTiles = {
    update()
    justBuildings.asReadOnlyView
  }
  def blockedByResources = {
    update()
    justMineralsAndGas.asReadOnlyView
  }
  def blockedbyMines = {
    update()
    justMines.asReadOnlyView
  }
  def blockedForResourceDeposit = {
    update()
    justBlockedForMainBuilding.mostRecent.getOrElse(emptyGrid)
  }
  def blockedByWorkerPaths = {
    update()
    justWorkerPaths.asReadOnlyView
  }
  def blockedByMobileUnits = {
    update()
    justBlockingMobiles.asReadOnlyView
  }
  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }
  def unblockBuilding_!(where: Area): Unit = {
    plannedBuildings.free_!(where)
  }
  universe.bases.register((base: Base) => {
    justWorkerPaths = evalWorkerPaths
  }, notifyForExisting = true)

  def tick(): Unit = {
  }
  private def evalWorkerPaths = {
    trace("Re-evaluation of worker paths")
    val ret = emptyGrid
    bases.bases.foreach { base =>
      base.myMineralGroup.foreach { group =>
        group.patches.foreach { patch =>
          base.mainBuilding.area.outline.foreach { outline =>
            patch.area.tiles.foreach { patchTile =>
              ret.block_!(outline, patchTile)
            }
          }
        }
      }
      base.myGeysirs.foreach { geysir =>
        base.mainBuilding.area.outline.foreach { tile1 =>
          geysir.area.outline.foreach { tile2 =>
            ret.block_!(tile1, tile2)
          }
        }
      }
    }
    ret
  }
  private def emptyGrid = world.map.emptyZoomed.mutableCopy
  private def update(): Unit = {
    if (lastUpdatePerformedInTick != universe.currentTick) {
      lastUpdatePerformedInTick = universe.currentTick

      justBuildings = evalOnlyBuildings
      justMines = evalOnlyMines
      justMineralsAndGas = evalOnlyResources
      justBlockingMobiles = evalOnlyMobileBlockingUnits
      justAddonLocations = evalPotentialAddonLocations

      withBuildings = evalWithBuildings
      withBuildingsAndResources = evalWithBuildingsAndResources
      withEverythingStaticBuildable = evalEverythingStaticBuildable
      withEverythingStaticWalkable = evalEverythingStaticWalkable
      withEverythingBlockingBuildable = evalEverythingBlockingBuildable
      withEverythingBlockingWalkable = evalEverythingBlockingWalkable

      ifNth(Primes.prime89) {
        // these are cpu heavy
        justAreasToDefend.prepareNextIfDone()
        justBlockedForMainBuilding.prepareNextIfDone()
        freeWalkableAndSafe.prepareNextIfDone()
        coveredAir.prepareNextIfDone()
        coveredGround.prepareNextIfDone()
        coveredCloaked.prepareNextIfDone()
      }
    }
  }
  private def evalWithBuildings = rawMapBuild.mutableCopy.or_!(justBuildings)
  private def evalWithBuildingsAndResources = justBuildings.mutableCopy.or_!(justMineralsAndGas)
  private def evalOnlyBuildings = evalOnlyUnits(ownUnits.allByType[Building])
  private def evalOnlyAreasToDefend = {
    evalOnlyUnitsAsync(ownUnits.allByType[Building], 8)
  }
  private def evalOnlyBlockedForMainBuildings = evalOnlyBlockedResourceAreas(
    ownUnits.allByType[Resource])
  private def evalPotentialAddonLocations = evalOnlyAddonAreas(ownUnits.allByType[CanBuildAddons])
  private def evalOnlyAddonAreas(units: TraversableOnce[CanBuildAddons]) = {
    val ret = emptyGrid
    units.foreach { b =>
      ret.block_!(b.addonArea)
    }
    ret
  }
  private def evalOnlyMobileBlockingUnits = evalOnlyMobileUnits(ownUnits.allByType[GroundUnit])
  private def evalOnlyMines = evalOnlyMobileUnits(ownUnits.allByType[SpiderMine])
  private def evalOnlyMobileUnits(units: TraversableOnce[GroundUnit]) = {
    val ret = emptyGrid
    units.foreach { b =>
      ret.block_!(b.currentTile)
    }
    ret
  }
  private def evalOnlyResources = evalOnlyUnits(
    ownUnits.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(ownUnits.allByType[Geysir]))

  private def evalOnlyUnits(units: TraversableOnce[StaticallyPositioned]) = {
    val ret = emptyGrid
    units.foreach { b =>
      val by = b.area
      ret.block_!(by)
    }
    ret
  }

  private def evalOnlyUnitsAsync(units: => TraversableOnce[StaticallyPositioned], growBy: Int) = {
    def areas = units.map(_.area)
    FutureIterator.feed(areas).andProduce { areas =>
      val ret = emptyGrid
      areas.foreach { a =>
        val by = a.growBy(growBy)
        ret.block_!(by)
      }
      ret.guaranteeImmutability
    }
  }

  private def evalOnlyBlockedResourceAreas(units: => TraversableOnce[Resource]) = {
    def areas = units.map(_.blockingAreaForMainBuilding)
    FutureIterator.feed(areas).andProduce { in =>
      val ret = emptyGrid
      in.foreach { area =>
        ret.block_!(area)
      }
      ret
    }
  }
  private def evalEverythingStaticBuildable = withBuildingsAndResources.mutableCopy
                                              .or_!(plannedBuildings)
                                              .or_!(justWorkerPaths)
                                              .or_!(rawMapBuild)
  private def evalEverythingStaticWalkable = withBuildingsAndResources.mutableCopy
                                             .or_!(plannedBuildings)
                                             .or_!(justWorkerPaths)
                                             .or_!(rawMapWalk2)
  private def evalEverythingBlockingBuildable = withEverythingStaticBuildable.mutableCopy
                                                .or_!(justBlockingMobiles)
  private def evalEverythingBlockingWalkable = withEverythingStaticWalkable.mutableCopy
                                               .or_!(justBlockingMobiles)

  private class EvalSafeInput {
    val base           = rawMapWalk.mutableCopy
    val buildings      = universe.enemyUnits.allBuildings.map(_.centerTile)
    val armedBuildings = universe.enemyUnits.allBuildingsWithWeapons.map(_.centerTile)
    val units          = universe.enemyUnits.allMobiles.filterNot(_.isHarmlessNow).map(_.currentTile)
  }

  private def areaOfCircles(trav: => TraversableOnce[Circle]) = {
    FutureIterator.feed {trav}.andProduce { in =>
      val base = emptyGrid.mutableCopy
      for (circle <- in; tile <- circle.asTiles) {
        base.block_!(tile)
      }
      base.guaranteeImmutability
    }

  }
  private def evalDetected = areaOfCircles {
    universe.ownUnits.allDetectors.map(_.detectionArea)
  }

  private def evalGroundDefended = areaOfCircles {
    universe.ownUnits.allWithGroundWeapon.map(_.inGroundWeaponRange)
  }

  private def evalAirDefended = areaOfCircles {
    universe.ownUnits.allWithAirWeapon.map(_.inAirWeaponRange)
  }

  private def evalSafe = {
    FutureIterator.feed(new EvalSafeInput).andProduce { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.buildings, 12, 3).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.units, 12, 5).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.armedBuildings, 12, 1).foreach(base.block_!)
      base.guaranteeImmutability
    }
  }
}

trait SubFinder {
  def find: Option[MapTilePosition]
}

class ConstructionSiteFinder(universe: Universe) {

  // initialisation happens in the main thread
  private val freeToBuildOn            = universe.mapLayers.buildableBlockedByNothingTiles
                                         .mutableCopy
                                         .or_!(
                                           universe.mapLayers.blockedByPotentialAddons.mutableCopy)
                                         .or_!(
                                           universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
  private val freeToBuildOnIgnoreUnits = universe.mapLayers.freeTilesForConstruction.mutableCopy
                                         .or_!(universe.mapLayers.blockedByPotentialAddons
                                               .mutableCopy)
                                         .or_!(
                                           universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
                                         .guaranteeImmutability

  private val outlineTouchCountArea = universe.mapLayers.blockedByBuildingTiles.mutableCopy
                                      .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
                                      .or_!(
                                        universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
                                      .guaranteeImmutability

  private val helper = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)
  def forResourceArea(resources: ResourceArea): SubFinder = {
    val size = Size(4 + 2, 3) // include space for comsat
    //main thread
    val grid = freeToBuildOn.or_!(universe.mapLayers.blockedForResourceDeposit)
    new SubFinder {
      override def find: Option[MapTilePosition] = {
        // background
        val possible = helper.iterateBlockSpiralClockWise(resources.center, 35)
                       .filter { candidate =>
                         def correctArea = {
                           val area = Area(candidate, size)
                           grid.inBounds(area) && grid.free(area)
                         }
                         def lineOfSight = {
                           def fromPatch = resources.patches.exists { mpg =>
                             mpg.patches.exists { p =>
                               AreaHelper.directLineOfSight(p.area.centerTile, candidate,
                                 universe.mapLayers.rawWalkableMap)
                             }
                           }
                           def fromGeysir = resources.geysirs.exists { geysir =>
                             geysir.area.outline.exists { p =>
                               AreaHelper.directLineOfSight(p, candidate,
                                 universe.mapLayers.rawWalkableMap)
                             }
                           }
                           fromPatch || fromGeysir
                         }
                         correctArea && lineOfSight
                       }
                       .toVector

        if (possible.isEmpty) {
          None
        } else {
          val closest = possible.minBy { elem =>
            val area = Area(elem, size)
            val distanceToPatches = resources.patches
                                    .map(_.patches.map(_.area.distanceTo(area)).sum).getOrElse(0.0)
            val distanceToGeysirs = resources.geysirs.map(_.area.distanceTo(area)).sum
            distanceToPatches + distanceToGeysirs
          }
          Some(closest)
        }
      }
    }
  }
  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[_ <: T]) = {
    // this happens in the background
    val unitType = building.toUnitType
    val necessarySize = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    val addonSize = Size(2, 2)
    val necessarySizeAddon = if (unitType.canBuildAddon) {
      Some(addonSize)
    } else None

    val withStreets = freeToBuildOn.mutableCopy

    helper.iterateBlockSpiralClockWise(near, 75).flatMap { upperLeft =>
      val area = Area(upperLeft, necessarySize)
      val addonArea = necessarySizeAddon.map(Area(area.lowerRight.movedBy(1, -1), _))
      def containsArea = freeToBuildOn.inBounds(area) &&
                         addonArea.map(freeToBuildOn.inBounds).getOrElse(true)
      def free = {
        val checkIfBlocksSelf = freeToBuildOnIgnoreUnits.mutableCopy
        checkIfBlocksSelf.block_!(area)
        addonArea.foreach(checkIfBlocksSelf.block_!)
        def areaFree = withStreets.free(area) &&
                       addonArea.map(withStreets.free).getOrElse(true)
        def outlineFree = area.growBy(1).outline.forall {freeToBuildOnIgnoreUnits.freeAndInBounds}
        def noLock = checkIfBlocksSelf.areaCountExpensive == freeToBuildOnIgnoreUnits.areaCount

        areaFree && (outlineFree || noLock)
      }
      if (containsArea && free) {
        val freeSurroundingTiles = area.growBy(1).outline
                                   .count(outlineTouchCountArea.freeAndInBounds)
        val distanceToCenter = area.centerTile.distanceTo(near)
        Some((upperLeft, distanceToCenter / 6.0, freeSurroundingTiles))
      } else {
        None
      }
    }.take(256)
    .minByOpt { case (_, b, c) => (b, c) }
    .map(_._1)
  }
}

