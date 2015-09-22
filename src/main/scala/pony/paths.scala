package pony

import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable

case class Path(waypoints: Seq[MapTilePosition])

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
            used ++= area
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
    ret
  }

  private def floodFill(start: MapTilePosition): mutable.BitSet = {
    val ret = mutable.BitSet.empty
    AreaHelper.traverseTilesOfArea(start, (x, y) => ret += x + y * baseOn.cols, baseOn)
    ret
  }

  def directLineOfSight(a: Area, b: MapTilePosition): Boolean = {
    a.outline.exists(p => directLineOfSight(p, b))
  }

  def directLineOfSight(a: Area, b: Area): Boolean = {
    b.outline.exists(p => directLineOfSight(a, p))
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

class MapLayers(override val universe: Universe) extends HasUniverse {

  private val rawMapWalk                 = world.map.walkableGrid
  private val rawMapBuild                = world.map.buildableGrid.mutableCopy
  private val plannedBuildings           = world.map.empty.zoomedOut.mutableCopy
  private var justBuildings              = evalOnlyBuildings
  private var justMines                  = evalOnlyMines
  private var justBlockedForMainBuilding = evalOnlyBlockedForMainBuildings
  private var justMineralsAndGas         = evalOnlyResources
  private var justWorkerPaths            = evalWorkerPaths
  private var justBlockingMobiles        = evalOnlyMobileBlockingUnits
  private var justAddonLocations         = evalPotentialAddonLocations
  private var withBuildings              = evalWithBuildings
  private var withBuildingsAndResources  = evalWithBuildingsAndResources
  private var withEverythingStatic       = evalEverythingStatic
  private var withEverythingBlocking     = evalEverythingBlocking
  private var lastUpdatePerformedInTick  = universe.currentTick
  def isOnIsland(tilePosition: MapTilePosition) = {
    val areaInQuestion = rawMapWalk.areas.find(_.free(tilePosition))
    !areaInQuestion.contains(rawWalkableMap.areas.maxBy(_.freeCount))
  }
  def rawWalkableMap = rawMapWalk
  def blockedByPotentialAddons = {
    update()
    justAddonLocations.asReadOnly
  }
  def blockedByPlannedBuildings = plannedBuildings.asReadOnly
  def freeBuildingTiles = {
    update()
    withEverythingStatic.asReadOnly
  }
  def reallyFreeBuildingTiles = {
    update()
    withEverythingBlocking.asReadOnly
  }
  def blockedByBuildingTiles = {
    update()
    justBuildings.asReadOnly
  }
  def blockedByResources = {
    update()
    justMineralsAndGas.asReadOnly
  }
  def blockedbyMines = {
    update()
    justMines.asReadOnly
  }
  def blockedForResourceDeposit = {
    update()
    justBlockedForMainBuilding.asReadOnly
  }
  def blockedByWorkerPaths = {
    update()
    justWorkerPaths.asReadOnly
  }
  def blockedByMobileUnits = {
    update()
    justBlockingMobiles.asReadOnly
  }
  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }
  def unblockBuilding_!(where: Area): Unit = {
    plannedBuildings.free_!(where)
  }
  def tick(): Unit = {
    ifNth(59) {
      // TODO only do this when a new base is built
      justWorkerPaths = evalWorkerPaths
    }
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
      justBlockedForMainBuilding = evalOnlyBlockedForMainBuildings
      justMineralsAndGas = evalOnlyResources
      justBlockingMobiles = evalOnlyMobileBlockingUnits
      justAddonLocations = evalPotentialAddonLocations
      withEverythingBlocking = evalEverythingBlocking
      withBuildings = evalWithBuildings
      withBuildingsAndResources = evalWithBuildingsAndResources
      withEverythingStatic = evalEverythingStatic
    }
  }
  private def evalWithBuildings = rawMapBuild.mutableCopy.or_!(justBuildings)
  private def evalWithBuildingsAndResources = justBuildings.mutableCopy.or_!(justMineralsAndGas)
  private def evalOnlyBuildings = evalOnlyUnits(ownUnits.allByType[Building])
  private def evalOnlyBlockedForMainBuildings = evalOnlyBlockedResourceAreas(ownUnits.allByType[Resource])
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
  private def evalOnlyResources = evalOnlyUnits(ownUnits.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(ownUnits.allByType[Geysir]))
  private def evalOnlyUnits(units: TraversableOnce[StaticallyPositioned]) = {
    val ret = emptyGrid
    units.foreach { b =>
      ret.block_!(b.area)
    }
    ret
  }
  private def evalOnlyBlockedResourceAreas(units: TraversableOnce[Resource]) = {
    val ret = emptyGrid
    units.foreach { b =>
      ret.block_!(b.blockingAreaForMainBuilding)
    }
    ret
  }
  private def evalEverythingStatic = withBuildingsAndResources.mutableCopy
                                     .or_!(plannedBuildings)
                                     .or_!(justWorkerPaths)
                                     .or_!(rawMapBuild)
  private def evalEverythingBlocking = withEverythingStatic.mutableCopy.or_!(justBlockingMobiles)
}

trait SubFinder {
  def find: Option[MapTilePosition]
}

class ConstructionSiteFinder(universe: Universe) {

  // initialisation happens in the main thread
  private val freeToBuildOn            = universe.mapLayers.reallyFreeBuildingTiles.mutableCopy
                                         .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
  private val freeToBuildOnIgnoreUnits = universe.mapLayers.freeBuildingTiles.mutableCopy
                                         .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
                                         .asReadOnlyCopy
  private val helper                   = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)
  def forResourceArea(resources: ResourceArea): SubFinder = {
    // we magically know this by now
    val size = Size(4, 3)
    //main thread
    val grid = freeToBuildOn.or_!(universe.mapLayers.blockedForResourceDeposit.mutableCopy)
    new SubFinder {
      override def find: Option[MapTilePosition] = {
        // background
        val possible = helper.blockSpiralClockWise(resources.center, 25)
                       .filter { candidate =>
                         def correctArea = {
                           val area = Area(candidate, size)
                           grid.includes(area) && grid.free(area)
                         }
                         def lineOfSight = {
                           resources.patches.exists { mpg =>
                             mpg.patches.exists { p =>
                               AreaHelper.directLineOfSight(p.area.centerTile, candidate,
                                 universe.mapLayers.rawWalkableMap)
                             }
                           }
                         }
                         correctArea && lineOfSight
                       }

        if (possible.isEmpty) {
          None
        } else {
          val closest = possible.minBy { elem =>
            val area = Area(elem, size)
            resources.patches.map(_.patches.map(_.area.distanceTo(area)).sum).get
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
    // add "streets" to make sure that we don't lock ourselves in too much
    withStreets.block_!(Line(near.movedBy(-20, 0), near.movedBy(20, 0)))
    withStreets.block_!(Line(near.movedBy(0, 20), near.movedBy(0, -20)))
    /*
        withStreets.block_!(Line(near.movedBy(-20, -20), near.movedBy(20, 20)))
        withStreets.block_!(Line(near.movedBy(-20, 20), near.movedBy(20, -20)))
    */

    helper.blockSpiralClockWise(near).find { upperLeft =>
      val area = Area(upperLeft, necessarySize)
      val addonArea = necessarySizeAddon.map(Area(area.lowerRight.movedBy(1, -1), _))
      def containsArea = freeToBuildOn.includes(area) && addonArea.map(freeToBuildOn.includes).getOrElse(true)
      def free = {
        val checkIfBlocksSelf = freeToBuildOnIgnoreUnits.mutableCopy
        checkIfBlocksSelf.block_!(area)
        addonArea.foreach(checkIfBlocksSelf.block_!)
        def areaFree = withStreets.free(area) &&
                       addonArea.map(withStreets.free).getOrElse(true)
        def noLock = checkIfBlocksSelf.areaCount == freeToBuildOnIgnoreUnits.areaCount
        areaFree && noLock
      }
      containsArea && free
    }
  }
}

