package pony

import pony.brain.{HasUniverse, Universe}

case class Path(waypoints: Seq[MapTilePosition])

class SimplePathFinder(baseOn:Grid2D) {
  def directLineOfSight(a: Area, b: MapTilePosition): Boolean = {
    a.outline.exists(p => directLineOfSight(p,b))
  }

  def directLineOfSight(a: MapTilePosition, b: MapTilePosition): Boolean = {
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
    if (baseOn.blocked(startX,startY)) {
      return false
    }
    if (dx > dy) {
      var fraction: Int = dy - (dx >> 1)
      while (startX != endX) {
        if (fraction >= 0) {
          startY += stepY
          fraction -= dx
        }
        startX += stepX
        fraction += dy
        if (baseOn.blocked(startX,startY)) {
          return false
        }
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
        if (baseOn.blocked(startX, startY)) {
          return false
        }
      }
    }
    true
  }
}

class MapLayers(override val universe: Universe) extends HasUniverse {
  private val simple               = world.map.walkableGridZoomed
  private val plannedBuildings     = world.map.empty.mutableCopy
  private var withBuildings        = evalWithBuildings
  private var withPlannedBuildings = evalWithPlannedBuildings
  def forBuildingConstruction = withPlannedBuildings.asReadOnly
  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }
  private def update(): Unit = {
    withBuildings = evalWithBuildings
    withPlannedBuildings = evalWithPlannedBuildings
  }
  private def evalWithBuildings = simple.mutableCopy.or_!(onlyBuildings)
  private def onlyBuildings = {
    val empty = world.map.empty.mutableCopy
    units.allByType[Building].foreach { b =>
      empty.block_!(b.area)
    }
    empty
  }
  private def evalWithPlannedBuildings = withBuildings.mutableCopy.or_!(plannedBuildings)

}

class ConstructionSiteFinder(universe: Universe) {
  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[_ <: T]) = {
    val necessaryArea = Size.shared(building.toUnitType.tileWidth(), building.toUnitType.tileWidth())
    val tryOnThis = universe.mapsLayers.forBuildingConstruction
    GeometryHelpers.blockSpiralClockWise(near).find(tryOnThis.free(_, necessaryArea))
  }
}

