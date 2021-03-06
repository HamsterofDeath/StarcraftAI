package pony

import pony.astar.{AStarSearch, GridNode2DInt, Heuristics}
import pony.brain.modules._
import pony.brain.{HasUniverse, Universe}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class MigrationPath(follow: Paths, override val universe: Universe)
  extends HasUniverse {

  private val remaining       = mutable.HashMap.empty[Mobile, ArrayBuffer[MapTilePosition]]
  private val counter         = mutable.HashMap.empty[Mobile, Int]
  private val helper          = new FormationHelper(universe, follow, 2, isGroundPath)
  private val atFormationStep = mutable.HashSet.empty[Mobile]

  onTick_!()

  private val creationTick = universe.currentTick

  override def onTick_!() = {
    super.onTick_!()
    remaining.retain { case (unit, path) =>
      unit.isInGame && path.nonEmpty
    }
  }

  def isGroundPath = follow.isGroundPath

  def originalDestination = follow.unsafeTarget

  def safeDestination = follow.requestedTargetSafe

  def targetFormationTiles = helper.formationTiles

  def renderDebugPaths(renderer: Renderer): Unit = {
    follow.renderDebug(renderer)
  }

  def isCloseToUnsafeTarget(m: Mobile) = {
    (remaining.get(m) match {
      case None if counter.contains(m) => true
      case Some(path) if path.isEmpty => true
      case _ => false
    }) && m.currentTile.distanceToIsLess(originalDestination, 7)
  }

  def stillActiveUnits = counter.keysIterator.filter(_.isInGame)

  def meetingStats = {
    atFormationStep.size -> remaining.size
  }

  def allCloseToDestination = {
    val old = creationTick + 24 < universe.currentTick
    old && remaining.isEmpty && stillActiveUnits.nonEmpty
  }

  def nextPositionFor(t: Mobile) = nextFor(t).map(_._1)

  def nextFor(t: Mobile) = {
    assertCalled()
    val index = counter.getOrElseUpdate(t, counter.size) % follow.pathCount
    val initialFullPath = follow.paths(index)
    val remainingPathForThisUnit = remaining.getOrElseUpdate(t,
      ArrayBuffer.empty ++= initialFullPath.waypoints)
    val closest = remainingPathForThisUnit.minByOpt(_.distanceSquaredTo(t.currentTile))
    val formationPoint = helper.assignedPosition(t)
    val canSeeFormationPoint = formationPoint.exists { target =>
      target.distanceToIsLess(t.currentTile, 15) &&
      mapLayers.rawWalkableMap.connectedByLine(target, t.currentTile)
    }
    def isOver30Percent = {
      remainingPathForThisUnit.size / initialFullPath.waypoints.size.toDouble <= 0.7
    }
    def manyArrived = atFormationStep.size.toDouble / counter.size >= 0.8
    def skipFormation = {
      t.currentTile.distanceSquaredTo(initialFullPath.unsafeTarget) <
      formationPoint.map(t.currentTile.distanceSquaredTo).getOrElse(10000)
    }

    def clearPath() = remainingPathForThisUnit.clear()
    if (manyArrived) {
      clearPath()
      Some(follow.unsafeTarget -> true)
    } else if (skipFormation) {
      clearPath()
      atFormationStep += t
      None // just wait
    } else if (canSeeFormationPoint) {
      clearPath()
      atFormationStep += t
      formationPoint.map(e => e -> isOver30Percent)
    } else {
      closest.map { c =>
        if (c.distanceSquaredTo(t.currentTile) <= 25) {
          remainingPathForThisUnit.removeUntilInclusive(_ == c)
        }
        c -> isOver30Percent
      }
    }
  }
}

case class Path(waypoints: Seq[MapTilePosition], solved: Boolean, solvable: Boolean,
                bestEffort: MapTilePosition, requestedTarget: MapTilePosition,
                unsafeTarget: MapTilePosition)(basedOn: Grid2D) {
  lazy    val length = {
    lengthOf(waypoints.iterator)
  }
  private val cache  = mutable.HashMap.empty[MapTilePosition, Option[Double]]

  def distanceToFinalTargetViaPath(from: MapTilePosition) = {
    cache.getOrElseUpdate(from, {
      closestWaypoint(from).map { first =>
        lengthOf(waypoints.iterator.dropWhile(_ != first)) + first.distanceTo(from)
      }
    })
  }

  def closestWaypoint(of: MapTilePosition) = {
    // not perfect, but good enough
    waypoints.iterator
    .filter(e => basedOn.connectedByLine(e, of))
    .minByOpt(_.distanceSquaredTo(of))
  }

  private def lengthOf(path: Iterator[MapTilePosition]) = {
    path.sliding(2, 1).map {
      case Seq(a, b) => a.distanceTo(b)
      case Seq(singleElement) => 0.0
    }.sum
  }

  def head = waypoints.head

  def isPerfectSolution = solved
}

case class Paths(paths: Seq[Path], isGroundPath: Boolean) {
  val pathCount           = paths.size
  val requestedTargetSafe = paths.headOption.map(_.requestedTarget).get
  val unsafeTarget        = paths.headOption.map(_.unsafeTarget).get
  val realisticTarget     = MapTilePosition.average(paths.map(_.bestEffort))
  val anyTarget           = paths.head.bestEffort

  def renderDebug(renderer: Renderer): Unit = {
    paths.foreach { singlePath =>
      var prev = Option.empty[MapTilePosition]
      singlePath.waypoints.zipWithIndex.foreach { case (tile, index) =>
        renderer.drawTextAtTile(s"P$index", tile)
        prev.foreach { p =>
          renderer.drawLineInTile(p, tile)
        }
        prev = Some(tile)
      }
    }
  }

  def toMigration(implicit universe: Universe) = new MigrationPath(this, universe)

  def isEmpty = paths.forall(_.waypoints.isEmpty)

  def minimalDistanceTo(tile: MapTilePosition) = {
    if (paths.isEmpty)
      0.0
    else
      math.sqrt(paths.iterator.flatMap(_.waypoints.map(_.distanceSquaredTo(tile))).min)
  }
}

object PathFinder {

  implicit def convBack(gn: GridNode2DInt): MapTilePosition = MapTilePosition
                                                              .shared(gn.getX, gn.getY)

  def on(map: Grid2D, isOnGround: Boolean) = {
    new PathFinder(map, isOnGround)
  }

  def to2DArray(grid: Grid2D) = {

    val asGrid = Array.ofDim[GridNode2DInt](grid.cols, grid.rows)
    implicit def conv(mtp: MapTilePosition): GridNode2DInt = {
      asGrid(mtp.x)(mtp.y)
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

class PathFinder(on: Grid2D, isOnGround: Boolean) {

  import PathFinder._

  def this(mapLayers: MapLayers, safe: Boolean, ground: Boolean) {
    this({
      (safe, ground) match {
        case (true, true) =>
          mapLayers.safeGround.guaranteeImmutability
        case (false, true) =>
          mapLayers.freeWalkableIgnoringMobiles.guaranteeImmutability
        case (false, false) =>
          mapLayers.emptyGrid.guaranteeImmutability
        case (true, false) =>
          mapLayers.safeAir.guaranteeImmutability
      }
    }, ground)
  }

  def findPath(from: MapTilePosition, to: MapTilePosition) = {
    findPaths(from, to, 1)
  }

  def findSimplePathNow(from: MapTilePosition, to: MapTilePosition,
                        tryFixPath: Boolean = true) = {
    findPathNow(from, to, 1, tryFixPath).flatMap(_.paths.headOption)
  }

  def findPathNow(from: MapTilePosition, to: MapTilePosition,
                  paths: Int = 1, tryFixPath: Boolean = true): Option[Paths] = {
    evalPath(from, to, paths, tryFixPath)
  }

  def findPaths(from: MapTilePosition, to: MapTilePosition, paths: Int = 10,
                tryFixPath: Boolean = true) = BWFuture {
    findPathNow(from, to, paths, tryFixPath)
  }

  private def evalPath(from: MapTilePosition, to: MapTilePosition, paths: Int,
                       tryFixPath: Boolean): Option[Paths] = {
    val fromFixed = {if (tryFixPath) on.nearestFree(from) else Some(from)}
    val toFixed = {
      if (tryFixPath) {
        fromFixed.flatMap { e =>
          on.areaOf(e).flatMap(_.nearestFreeNoGap(to))
        }
      } else {Some(to)}
    }
    warn(s"Could not fix start $from", fromFixed.isEmpty)
    warn(s"Could not fix goal $to", toFixed.isEmpty)
    val unsafeTarget = to
    for (a <- fromFixed; b <- toFixed) yield spawn.findPaths(a, b, paths, unsafeTarget)
  }

  def spawn = new AstarPathFinder(to2DArray(on), isOnGround)

  class AstarPathFinder(grid: Array[Array[GridNode2DInt]], isOnGround: Boolean) {

    def basedOn = on

    def findPaths(from: MapTilePosition, to: MapTilePosition, width: Int,
                  unsafeTarget: MapTilePosition) = {
      trace(s"Searching path from $from to $to", tick > 10)
      val finder = new AStarSearch[GridNode2DInt](from, to)
      var first = Option.empty[Path]
      def pathFrom(seq: Seq[MapTilePosition]) = {
        Path(seq, finder.isSolved, !finder.isUnsolvable, finder.getTargetOrNearestReachable, to,
          unsafeTarget)(on)
      }
      val paths = (0 until width).iterator.map { _ =>
        finder.performSearch()
        val waypoints = finder.getFullSolution
                        .asScala
                        .map(e => e: MapTilePosition)
                        .sliding(1, 4)
                        .flatten
                        .toVector
        //block the path, then search again to get streets
        finder.getFullSolution.asScala.drop(15).dropRight(10).foreach(_.remove())
        if (first.isEmpty) first = Some(
          pathFrom(waypoints :+ (finder.getTargetOrNearestReachable: MapTilePosition)))
        waypoints
      }.takeWhile { candidate =>
        candidate.forall { pointOnLine =>
          first.get.waypoints.exists { old =>
            on.connectedByLine(old, pointOnLine)
          }
        }
      }.toVector
      Paths(paths.map(pathFrom), isOnGround)
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
    val ret = areas.sortBy(-_.freeCount)
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

  override def onTick_!(): Unit = {
    super.onTick_!()
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
                                           friendly: Boolean,
                                           customFilter: T => Boolean = (_: T) => true):
  Traversable[T] = {
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
            val byType = mobiles.iterator
                         .filter(filter.isInstance)
                         .filter(e => customFilter(e.asInstanceOf[T]))
            byType.foreach { e =>
              f(e.asInstanceOf[T])
            }
          }
        }
      }
    }
  }

  private def on(hostile: Boolean) = if (hostile) enemyUnits else myUnits
}

class MapLayers(override val universe: Universe) extends HasUniverse {
  private val myCurrentSafeInput = oncePerTick {
    new EvalSafeInput
  }

  type AreaFromCircle = FutureIterator[TraversableOnce[Circle], Grid2D]
  private val rawMapWalk        = world.map.walkableGrid
  private val empty             = world.map.walkableGrid.emptySameSize(false)
                                  .guaranteeImmutability
  private val full              = empty.reverseView
  private val rawMapWalkMutable = world.map.walkableGrid.mutableCopy
  private val rawMapBuild       = world.map.buildableGrid.mutableCopy
  private val plannedBuildings  = world.map.empty.zoomedOut.mutableCopy

  private val mapKey = uniqueKey

  private def register[T <: Grid2D](gen: => T) = {
    val lazyVal = explicitly(mapKey, gen, synchronize = true)
    lazyVal.get
    lazyVal
  }

  private val justBuildings                   = register(evalOnlyBuildings)
  private val justMines                       = register(evalOnlyMines)
  private val justMineralsAndGas              = register(evalOnlyResources)
  private val justWorkerPaths                 = register(evalWorkerPaths)
  private val justBlockingMobiles             = register(evalOnlyMobileBlockingUnits)
  private val justAddonLocations              = register(evalPotentialAddonLocations)
  private val withBuildingsAndResources       = register(evalWithBuildingsAndResources)
  private val withEverythingStaticBuildable   = register(evalEverythingStaticBuildable)
  private val withEverythingStaticWalkable    = register(evalEverythingStaticWalkable)
  private val withEverythingBlockingBuildable = register(evalEverythingBlockingBuildable)
  private val withEverythingBlockingWalkable  = register(evalEverythingBlockingWalkable)

  private val cpuHeavy = ArrayBuffer.empty[FutureIterator[_, Grid2D]]

  implicit class RichFuture(val f: FutureIterator[_, Grid2D]) {
    def registeredAs(name: String) = {
      cpuHeavy += f.named(name)
      f.setupRecalcHint(Primes.prime43)
    }
  }

  // too cpu heavy to be done in the main thread
  private val justAreasToDefend                    = evalOnlyAreasToDefend
                                                     .registeredAs("Areas to defend")
  private val coveredByPsiStorm                    = evalUnderPsiStorm
                                                     .registeredAs("Unser psi storm")
                                                     .setupRecalcHint(Primes.prime2)
  private val justBlockedForMainBuilding           = evalOnlyBlockedForMainBuildings
                                                     .registeredAs("Main building blocked")
  private val coveredByDangerousUnits              = evalDangerousOnGround.registeredAs("Dangerous")
  private val coveredByHostileLongRangeGroundUnits = evalHostileLongRangeGroundUnits
                                                     .registeredAs("Long range ground hostiles")
  private val coveredByHostileCloakedGroundUnits   = evalHostileCloakedGroundUnits
                                                     .registeredAs("Cloaked ground hostiles")
                                                     .setupRecalcHint(Primes.prime11)
  private val coveredByHostileCloakedAirUnits      = evalHostileCloakedAirUnits
                                                     .registeredAs("Cloaked air hostiles")
                                                     .setupRecalcHint(Primes.prime11)
  private val coveredByHostileLongRangeAirUnits    = evalHostileLongRangeAirUnits
                                                     .registeredAs("Long range air hostiles")
  private val coveredByAnythingWithGroundWeapons   = evalSlightlyDangerousForGroundUnits
                                                     .registeredAs("Ground hostiles")

  private val coveredByAnythingWithAirWeapons = evalSlightlyDangerousForAirUnits
                                                .registeredAs("Air hostiles")
  private val coveredByOwnDetectors           = evalDetected.registeredAs("detected (self)")
  private val coveredByOwnGround              = evalGroundDefended
                                                .registeredAs("Covered by ground (self)")
  private val coveredbyOwnAir                 = evalAirDefended
                                                .registeredAs("Covered by air (self)")
  private val exposedToCloaked                = evalExposedToCloakedUnits
                                                .registeredAs("Exposed to cloaked units (self)")
  private val justBlockingMobilesExtended     = evalOnlyMobileBlockingUnitsExtended
                                                .registeredAs("Ground mobiles (self)")

  // initializion order mess
  private val walkableSafe                           = register(evalWalkableSafe)
  private val airSafe                                = register(evalAirSafe)
  private val withEverythingSlightlyDangerousBlocked = register(evalSlightlyDangerousForAnyUnit)
  private val withAvoidSuggestionGroundBlocked       = register(evalAvoidanceSuggestionGround)
  private val withAvoidSuggestionAirBlocked          = register(evalAvoidanceSuggestionAir)

  private var lastUpdatePerformedInTick = universe.currentTick

  def isOnIsland(tilePosition: MapTilePosition) = {
    val areaInQuestion = rawMapWalk.areaOf(tilePosition)
    val maxArea = rawWalkableMap.areas.sortBy(-_.freeCount)
    if (maxArea.size > 1) {
      val main = maxArea.head
      val second = maxArea(1)
      val assumeIslandMap = second.freeCount * 2 > main.freeCount
      assumeIslandMap || !areaInQuestion.contains(main)
    } else false

  }

  def rawWalkableMap = rawMapWalk

  def slightlyDangerousAsBlocked = withEverythingSlightlyDangerousBlocked.get

  def avoidanceSuggestionGround = withAvoidSuggestionGroundBlocked.get

  def avoidanceSuggestionAir = withAvoidSuggestionAirBlocked.get

  def defendedTiles = {
    justAreasToDefend.getOrElse(empty)
  }

  def exposedToCloakedUnits = {
    exposedToCloaked.getOrElse(full)
  }

  def blockedByPotentialAddons = {
    justAddonLocations.asReadOnlyView
  }

  def underPsiStorm = coveredByPsiStorm.getOrElse(emptyGrid)

  def blockedByPlannedBuildings = plannedBuildings.asReadOnlyView

  def coveredByDetectors = coveredByOwnDetectors.getOrElse(emptyGrid)

  def coveredByOwnGroundUnits = coveredByOwnGround.getOrElse(emptyGrid)

  def coveredByOwnAirUnits = coveredbyOwnAir.getOrElse(emptyGrid)

  def dangerousAsBlocked = coveredByDangerousUnits.getOrElse(emptyGrid)

  def coveredByEnemyLongRangeGroundAsBlocked = coveredByHostileLongRangeGroundUnits
                                               .getOrElse(emptyGrid)

  def coveredByEnemyCloakedGroundAsBlocked = coveredByHostileCloakedGroundUnits
                                             .getOrElse(emptyGrid)

  def coveredByEnemyCloakedAirAsBlocked = coveredByHostileCloakedAirUnits
                                          .getOrElse(emptyGrid)

  def coveredByEnemyLongRangeAirAsBlocked = coveredByHostileLongRangeAirUnits
                                            .getOrElse(emptyGrid)

  def slightlyDangerousForGroundAsBlocked = coveredByAnythingWithGroundWeapons
                                            .getOrElse(emptyGrid)

  def slightlyDangerousForAirAsBlocked = coveredByAnythingWithAirWeapons
                                         .getOrElse(emptyGrid)

  def freeTilesForConstruction = {
    withEverythingStaticBuildable.asReadOnlyView
  }

  def buildableBlockedByNothingTiles = {
    withEverythingBlockingBuildable.asReadOnlyView
  }

  def freeWalkableTiles = {
    withEverythingBlockingWalkable.asReadOnlyView
  }

  def freeWalkableIgnoringMobiles = {
    withEverythingStaticWalkable.asReadOnlyView
  }

  def blockedByBuildingTiles = {
    justBuildings.asReadOnlyView
  }

  def blockedByResources = {
    justMineralsAndGas.asReadOnlyView
  }

  def blockedByMines = {
    justMines.asReadOnlyView
  }

  def blockedForResourceDeposit = {
    justBlockedForMainBuilding.getOrElse(empty)
  }

  def blockedByWorkerPaths = {
    justWorkerPaths.asReadOnlyView
  }

  def blockedByMobileUnits = {
    justBlockingMobiles.asReadOnlyView
  }

  def blockedByMobileUnitsExtended = {
    justBlockingMobilesExtended.getOrElse(empty)
  }

  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }

  def unblockBuilding_!(where: Area): Unit = {
    plannedBuildings.free_!(where)
  }

  def tick(): Unit = {
    super.onTick_!()
    update()
  }

  def emptyGrid = world.map.emptyZoomed

  def safeGround = walkableSafe

  def safeAir = airSafe

  private def evalWorkerPaths = {
    trace("Re-evaluation of worker paths")
    val ret = emptyCopy
    bases.allBases.foreach { base =>
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

  private def update(): Unit = {
    if (lastUpdatePerformedInTick != universe.currentTick) {
      lastUpdatePerformedInTick = universe.currentTick

      invalidate(mapKey)

      cpuHeavy.iterator
      .filter(_.triggerRecalcOn(currentTick))
      .foreach(_.prepareNextIfDone())
    }
  }

  private def evalWithBuildingsAndResources = justBuildings.mutableCopy.or_!(justMineralsAndGas)

  private def evalOnlyBuildings = evalOnlyUnits(ownUnits.allByType[Building])

  private def evalOnlyAreasToDefend = {
    evalOnlyUnitsAsync(ownUnits.allByType[Building].filterNot(_.isInstanceOf[DetectorBuilding]), 8)
  }

  private def evalOnlyUnitsAsync(units: => TraversableOnce[StaticallyPositioned], growBy: Int) = {
    def areas = units.map(_.area)
    FutureIterator.feed(areas.toVector).produceAsync { unitAreas =>
      val ret = emptyCopy
      unitAreas.foreach { a =>
        val by = a.growBy(growBy)
        ret.block_!(by)
      }
      ret.guaranteeImmutability
    }
  }

  private def emptyCopy = world.map.emptyZoomed.mutableCopy

  private def evalOnlyBlockedForMainBuildings = evalOnlyBlockedResourceAreas(
    ownUnits.allByType[Resource])

  private def evalOnlyBlockedResourceAreas(units: => TraversableOnce[Resource]) = {
    def areas = units.map(_.blockingAreaForMainBuilding)
    FutureIterator.feed(areas).produceAsync { in =>
      val ret = emptyCopy
      in.foreach { area =>
        ret.block_!(area)
      }
      ret.guaranteeImmutability
    }
  }

  private def evalPotentialAddonLocations = evalOnlyAddonAreas(ownUnits.allByType[CanBuildAddons])

  private def evalOnlyAddonAreas(units: TraversableOnce[CanBuildAddons]) = {
    val ret = emptyCopy
    units.foreach { b =>
      ret.block_!(b.addonArea)
    }
    ret
  }

  private def evalOnlyMobileBlockingUnits = evalOnlyMobileUnits(
    ownUnits.allByType[GroundUnit].iterator.filter(_.onGround))

  private def evalOnlyMobileBlockingUnitsExtended = FutureIterator.feed(blockedByMobileUnits)
                                                    .produceAsync { in =>
                                                      in.mutableCopy.addOutlineToBlockedTiles_!()
                                                      .guaranteeImmutability
                                                    }

  private def evalOnlyMines = evalOnlyMobileUnits(ownUnits.allByType[SpiderMine])

  private def evalOnlyMobileUnits(units: TraversableOnce[GroundUnit]) = {
    val ret = emptyCopy
    units.foreach { b =>
      ret.block_!(b.currentTile)
    }
    ret
  }

  private def evalOnlyResources = evalOnlyUnits(
    ownUnits.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(ownUnits.allByType[Geysir]))

  private def evalOnlyUnits(units: TraversableOnce[StaticallyPositioned]) = {
    val ret = emptyCopy
    units.foreach { b =>
      val by = b.area
      ret.block_!(by)
    }
    ret
  }

  private def evalEverythingStaticBuildable = withBuildingsAndResources.mutableCopy
                                              .or_!(plannedBuildings)
                                              .or_!(justWorkerPaths)
                                              .or_!(rawMapBuild)

  private def evalEverythingStaticWalkable = withBuildingsAndResources.mutableCopy
                                             .or_!(plannedBuildings)
                                             .or_!(rawMapWalkMutable)

  private def evalEverythingBlockingBuildable = withEverythingStaticBuildable.mutableCopy
                                                .or_!(justBlockingMobiles)

  private def evalSlightlyDangerousForAnyUnit = slightlyDangerousForGroundAsBlocked
                                                .or(slightlyDangerousForAirAsBlocked)

  private def evalAvoidanceSuggestionGround = coveredByEnemyLongRangeGroundAsBlocked
                                              .or(coveredByEnemyCloakedGroundAsBlocked)

  private def evalAvoidanceSuggestionAir = coveredByEnemyLongRangeAirAsBlocked
                                           .or(coveredByEnemyCloakedAirAsBlocked)

  private def evalEverythingBlockingWalkable = withEverythingStaticWalkable.mutableCopy
                                               .or_!(justBlockingMobiles)

  private def evalWalkableSafe = withEverythingStaticWalkable
                                 .or(dangerousAsBlocked)

  private def evalAirSafe = emptyCopy
                            .or_!(slightlyDangerousForAirAsBlocked.mutableCopy)
                            .asReadOnlyView

  private def evalDetected = areaOfCircles {
    universe.ownUnits.allDetectors.map(_.detectionArea)
  }

  private def areaOfCircles(trav: => TraversableOnce[Circle]): AreaFromCircle = {
    areaOfCircles(block = true)(trav)
  }

  private def areaOfCircles(block: Boolean)(trav: => TraversableOnce[Circle]): AreaFromCircle = {
    FutureIterator.feed(trav).produceAsync { in =>
      val base = if (block) emptyCopy.mutableCopy else emptyCopy.mutableCopy.invertedMutable
      for (circle <- in; tile <- circle.asTiles) {
        if (block) {
          base.block_!(tile)
        } else {
          base.free_!(tile)
        }
      }
      base.guaranteeImmutability
    }
  }

  private def evalExposedToCloakedUnits = areaOfCircles(block = false) {
    universe.ownUnits.allDetectors.map(_.detectionArea)
  }

  private def evalGroundDefended = areaOfCircles {
    universe.ownUnits.allWithGroundWeapon.map(_.inGroundWeaponRange)
  }

  private def evalAirDefended = areaOfCircles {
    universe.ownUnits.allWithAirWeapon.map(_.inAirWeaponRange)
  }

  private def evalDangerousOnGround = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.buildings, 12, 3).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 5).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsGround, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def safeInputForCurrentTick = {
    myCurrentSafeInput.get
  }

  private def evalHostileLongRangeGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections
      .tilesInCircleWithRange(in.enemy.longRangeGroundCoveringUnits, 7, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileCloakedGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.cloakedGroundCoveringUnits, 4, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalUnderPsiStorm = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.own.ownUnitsUnderPsi, 2, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileCloakedAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.cloakedAirCoveringUnits, 4, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileLongRangeAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.longRangeAirCoveringUnits, 7, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalSlightlyDangerousForGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 1).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsGround, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalSlightlyDangerousForAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 1).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsAir, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private class EvalSafeInput {
    private val baseTemplate = emptyCopy

    def base = baseTemplate.mutableCopy

    val enemy = new {
      val buildings            = universe.enemyUnits.allBuildings.map(_.centerTile)
      val armedBuildingsGround = universe.enemyUnits.allBuildingsWithGroundWeapons.map(_.centerTile)
      val armedBuildingsAir    = universe.enemyUnits.allBuildingsWithAirWeapons.map(_.centerTile)

      val units = {
        universe.enemyUnits.allMobiles.filterNot(_.isHarmlessNow).map(_.currentTile)
      }

      val longRangeGroundCoveringUnits = {
        universe.enemyUnits.allWithGroundWeapon.filterNot(_.isHarmlessNow)
        .filter(_.groundRangeTiles >= 7)
        .map { e =>
          e.centerTile -> e.groundRangeTiles
        }
      }
      val cloakedGroundCoveringUnits   = {
        universe.enemyUnits.allWithGroundWeapon.filterNot(_.isHarmlessNow)
        .collect { case cd: CanHide if cd.isHidden => cd }
        .map { e =>
          e.centerTile -> e.groundRangeTiles
        }
      }
      val cloakedAirCoveringUnits      = {
        universe.enemyUnits.allWithAirWeapon.filterNot(_.isHarmlessNow)
        .collect { case cd: CanHide if cd.isHidden => cd }
        .map { e =>
          e.centerTile -> e.airRangeTiles
        }
      }
      val longRangeAirCoveringUnits    = {
        universe.enemyUnits.allWithAirWeapon.filterNot(_.isHarmlessNow)
        .filter(_.airRangeTiles >= 7)
        .map { e =>
          e.centerTile -> e.airRangeTiles
        }
      }
    }

    val own = new {
      val ownUnitsUnderPsi = {
        universe.ownUnits.allMobiles
        .filter(_.wasUnderPsiStormSince(48))
        .flatMap(_.lastKnownStormPosition)
      }
    }
  }
}

trait SubFinder {
  def find: Option[MapTilePosition]
}

class ConstructionSiteFinder(universe: Universe) {

  // initialisation happens in the main thread
  private val freeToBuildOn            = {
    universe.mapLayers.buildableBlockedByNothingTiles
    .mutableCopy
    .or_!(
      universe.mapLayers.blockedByPotentialAddons.mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
  }
  private val freeToBuildOnIgnoreUnits = {
    universe.mapLayers.freeTilesForConstruction.mutableCopy
    .or_!(universe.mapLayers.blockedByPotentialAddons
          .mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
    .guaranteeImmutability
  }

  private val outlineTouchCountArea = {
    universe.mapLayers.blockedByBuildingTiles.mutableCopy
    .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
    .guaranteeImmutability
  }

  private val helper = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)

  def forResourceArea(resources: ResourceArea): SubFinder = {
    val size = Size(4 + 2, 3) // include space for comsat
    //main thread
    val grid = freeToBuildOn.or_!(universe.mapLayers.blockedForResourceDeposit.mutableCopy)
    new SubFinder {
      override def find: Option[MapTilePosition] = {
        // background
        val possible = {
          helper.iterateBlockSpiralClockWise(resources.center, 35)
          .filter { candidate =>
            def correctArea = {
              val area = Area(candidate, size)
              grid.inBounds(area) && grid.free(area)
            }
            def lineOfSight = {
              def fromPatch = resources.allPatchTiles.exists { p =>
                AreaHelper.directLineOfSight(p, candidate,
                  universe.mapLayers.rawWalkableMap)
              }
              def fromGeysir = resources.allGeysirTiles.exists { p =>
                AreaHelper.directLineOfSight(p, candidate,
                  universe.mapLayers.rawWalkableMap)
              }
              fromPatch || fromGeysir
            }
            correctArea && lineOfSight
          }
          .toVector
        }

        if (possible.isEmpty) {
          None
        } else {
          val closest = possible.minBy { elem =>
            val area = Area(elem, size)
            val distanceToPatches = resources.allPatchTiles.map(e => area.distanceTo(e)).sum
            val distanceToGeysirs = resources.allGeysirTiles.map(e => area.distanceTo(area)).sum
            distanceToPatches + distanceToGeysirs
          }
          Some(closest)
        }
      }
    }
  }

  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[_ <: T], maxRange: Int = 75,
                                 bestOfN: Int = 256) = {
    // this happens in the background
    val unitType = building.toUnitType
    val necessarySize = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    val addonSize = Size(2, 2)
    val necessarySizeAddon = if (unitType.canBuildAddon) {
      Some(addonSize)
    } else None

    val withStreets = freeToBuildOn.mutableCopy

    helper.iterateBlockSpiralClockWise(near, maxRange).flatMap { upperLeft =>
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
    }.take(bestOfN)
    .minByOpt { case (_, b, c) => (b, c) }
    .map(_._1)
  }
}

