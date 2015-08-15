package pony

import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable

case class Path(waypoints: Seq[MapTilePosition])

class AreaHelper(source: Grid2D) {
  private val baseOn = source.ensureContainsBlocked

  def findFreeAreas = {
    val areas = mutable.ArrayBuffer.empty[collection.Set[Int]]
    baseOn.allFree.foreach { p =>
      val areaAlreadyKnown = areas.exists(_.contains(p.x + p.y * baseOn.cols))
      if (!areaAlreadyKnown) {
        val area = floodFill(p)
        if (area.nonEmpty) {
          areas += area
        }
      }
    }
    val ret = areas.toSeq.map { data =>
      val asGrid = new Grid2D(baseOn.cols, baseOn.rows, data, false)
      val flipped = asGrid.ensureContainsBlocked
      flipped
    }
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
    AreaHelper.traverseTilesOfLine(a, b, (x, y) => {
      if (baseOn.blocked(x, y)) Some(true) else None
    }, true)
  }
}

object AreaHelper {

  def traverseTilesOfLine[T](a: MapTilePosition, b: MapTilePosition, f: (Int, Int) => T): Unit = {
    traverseTilesOfLine(a, b, (x, y) => {f(x, y); None}, None)
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

  private val rawMapWalk                = world.map.walkableGridZoomed
  private val rawMapBuild               = world.map.buildableGrid
  private val plannedBuildings          = world.map.empty.mutableCopy
  private var justBuildings             = evalOnlyBuildings
  private var justMineralsAndGas        = evalOnlyResources
  private var justWorkerPaths           = evalWorkerPaths
  private var withBuildings             = evalWithBuildings
  private var withBuildingsAndResources = evalWithBuildingsAndResources
  private var withEverything            = evalEverything

  def freeBuildingTiles = withEverything.asReadOnly

  def blockedByBuildingTiles = justBuildings.asReadOnly

  def blockedByResources = justMineralsAndGas.asReadOnly

  def blockedByWorkerPaths = justWorkerPaths.asReadOnly

  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }

  def tick(): Unit = {
    update()
  }

  private def update(): Unit = {
    justBuildings = evalOnlyBuildings
    justMineralsAndGas = evalOnlyResources
    ifNth(60) {
      justWorkerPaths = evalWorkerPaths
    }

    withBuildings = evalWithBuildings
    withBuildingsAndResources = evalWithBuildingsAndResources
    withEverything = evalEverything
  }

  private def evalWithBuildings = rawMapBuild.mutableCopy.or_!(justBuildings)
  private def evalWithBuildingsAndResources = justBuildings.or_!(justMineralsAndGas)
  private def evalOnlyBuildings = evalOnlyUnits(units.allByType[Building])
  private def evalOnlyUnits(units: TraversableOnce[StaticallyPositioned]) = {
    val ret = world.map.empty.mutableCopy
    units.foreach { b =>
      ret.block_!(b.area)
    }
    ret
  }
  private def evalOnlyResources = evalOnlyUnits(units.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(units.allByType[Geysir]))
  private def evalEverything = withBuildingsAndResources.mutableCopy.or_!(plannedBuildings).or_!(justWorkerPaths)

  private def evalWorkerPaths = {
    trace("Re-evaluation of worker paths")
    val ret = world.map.empty.mutableCopy
    bases.bases.foreach { base =>
      base.myMineralGroup.foreach { group =>
        group.patches.foreach { patch =>
          base.mainBuilding.area.outline.foreach { outline =>
            patch.area.tiles.foreach { patchTile =>
              ret.blockLine_!(outline, patchTile)
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
  private val tryOnThis = universe.mapsLayers.freeBuildingTiles.mutableCopy
  private val helper    = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)

  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[_ <: T]) = {
    // this happens in the background
    val unitType = building.toUnitType
    val necessaryArea = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    helper.blockSpiralClockWise(near).find(tryOnThis.free(_, necessaryArea))
  }
}

