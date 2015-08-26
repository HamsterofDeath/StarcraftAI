package pony

import pony.brain.{HasUniverse, Universe}

import scala.collection.JavaConverters._
import scala.collection.mutable

case class Path(waypoints: Seq[MapTilePosition])

class AreaHelper(source: Grid2D) {
  private val baseOn = source.ensureContainsBlocked

  def findFreeAreas = {
    val areas = mutable.ArrayBuffer.empty[Grid2D]
    baseOn.allFree.foreach { p =>
      val areaAlreadyKnown = areas.exists(_.free(p.x, p.y))
      if (!areaAlreadyKnown) {
        val area = floodFill(p)
        if (area.nonEmpty) {
          areas += {
            val asGrid = new Grid2D(baseOn.cols, baseOn.rows, area, false)
            val flipped = asGrid.ensureContainsBlocked
            flipped
          }
        }
      }
    }
    val ret = areas.toSeq
    assert({
      val before = baseOn.freeCount
      val after = ret.map(_.freeCount)
      before == after.sum
    }, s"Math is broken")
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

  private val rawMapWalk                = world.map.walkableGrid
  private val rawMapBuild               = world.map.buildableGrid.mutableCopy
  private val plannedBuildings          = world.map.empty.zoomedOut.mutableCopy
  private var justBuildings             = evalOnlyBuildings
  private var justMineralsAndGas        = evalOnlyResources
  private var justWorkerPaths           = evalWorkerPaths
  private var justBlockingMobiles       = evalOnlyMobileBlockingUnits
  private var justAddonLocations        = evalPotentialAddonLocations
  private var withBuildings             = evalWithBuildings
  private var withBuildingsAndResources = evalWithBuildingsAndResources
  private var withEverythingStatic      = evalEverythingStatic
  private var withEverythingBlocking    = evalEverythingBlocking
  private var lastUpdatePerformedInTick = universe.currentTick
  def isOnIsland(tilePosition: MapTilePosition) = {

    val others = world.nativeGame
                 .getPlayers
                 .asScala
                 .map(_.getStartLocation)
                 .map { p => p -> rawMapWalk.areas.find(_.free(p)) }
                 .filter(_._2.exists(_.free(tilePosition)))
    others.size <= 1 // one or less starting positions = "island"
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
  private def update(): Unit = {
    if (lastUpdatePerformedInTick != universe.currentTick) {
      lastUpdatePerformedInTick = universe.currentTick
      justBuildings = evalOnlyBuildings
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
  private def evalOnlyBuildings = evalOnlyUnits(units.allByType[Building])
  private def evalPotentialAddonLocations = evalOnlyAddonAreas(units.allByType[CanBuildAddons])
  private def evalOnlyAddonAreas(units: TraversableOnce[CanBuildAddons]) = {
    val ret = world.map.emptyZoomed.mutableCopy
    units.foreach { b =>
      ret.block_!(b.addonArea)
    }
    ret
  }
  private def evalOnlyMobileBlockingUnits = evalOnlyMobileUnits(units.allByType[GroundUnit])
  private def evalOnlyMobileUnits(units: TraversableOnce[GroundUnit]) = {
    val ret = world.map.emptyZoomed.mutableCopy
    units.foreach { b =>
      ret.block_!(b.currentTile)
    }
    ret
  }
  private def evalOnlyResources = evalOnlyUnits(units.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(units.allByType[Geysir]))
  private def evalOnlyUnits(units: TraversableOnce[StaticallyPositioned]) = {
    val ret = world.map.emptyZoomed.mutableCopy
    units.foreach { b =>
      ret.block_!(b.area)
    }
    ret
  }
  private def evalEverythingStatic = withBuildingsAndResources.mutableCopy
                                     .or_!(plannedBuildings)
                                     .or_!(justWorkerPaths)
                                     .or_!(rawMapBuild)
  private def evalEverythingBlocking = withEverythingStatic.mutableCopy.or_!(justBlockingMobiles)
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
    val ret = world.map.emptyZoomed.mutableCopy
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
    }
    ret
  }
}

class ConstructionSiteFinder(universe: Universe) {
  // initialisation happens in the main thread
  private val withoutStreets = universe.mapLayers.reallyFreeBuildingTiles.mutableCopy
                               .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
  private val helper         = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)

  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[_ <: T]) = {
    // this happens in the background
    val unitType = building.toUnitType
    val necessarySize = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    val addonSize = Size(2, 2)
    val necessarySizeAddon = if (unitType.canBuildAddon) {
      Some(addonSize)
    } else None

    val withStreets = withoutStreets.mutableCopy
    // add "streets" to make sure that we don't lock ourselves in too much
    withStreets.block_!(Line(near.movedBy(-20, 0), near.movedBy(20, 0)))
    withStreets.block_!(Line(near.movedBy(0, 20), near.movedBy(0, -20)))
    withStreets.block_!(Line(near.movedBy(-20, -20), near.movedBy(20, 20)))
    withStreets.block_!(Line(near.movedBy(-20, 20), near.movedBy(20, -20)))

    helper.blockSpiralClockWise(near).find { upperLeft =>
      val area = Area(upperLeft, necessarySize)
      val addonArea = necessarySizeAddon.map(Area(area.lowerRight.movedBy(1, -1), _))
      def containsArea = withoutStreets.includes(area) && addonArea.map(withoutStreets.includes).getOrElse(true)
      def free = {
        val checkIfBlocksSelf = withoutStreets.mutableCopy
        checkIfBlocksSelf.block_!(area)
        addonArea.foreach(checkIfBlocksSelf.block_!)
        def areaFree = withStreets.free(area) &&
                       addonArea.map(withStreets.free).getOrElse(true)
        def noLock = checkIfBlocksSelf.areaCount == withoutStreets.areaCount
        areaFree && noLock
      }
      containsArea && free
    }
  }
}

