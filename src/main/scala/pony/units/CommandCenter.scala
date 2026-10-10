package pony
package units

import pony.geometry.{Area, MapTilePosition}

import bwapi.{Unit => APIUnit, _}

class CommandCenter(unit: APIUnit)
    extends AnyUnit(unit) with MainBuilding with CanBuildAddons with TerranBuilding {
  var relocating                                                = false
  override def canBuild[T <: Mobile](typeOfUnit: Class[? <: T]) =
    !relocating && !isFloating && super.canBuild(typeOfUnit)
  // Other buildings are static; this depot deliberately changes its resource field after lifting.
  override def tilePosition = {
    val p = nativeUnit.getTilePosition
    MapTilePosition.shared(p.getX, p.getY)
  }
  override def area      = Area(tilePosition, size)
  override def areaOnMap = mapLayers.rawWalkableMap.areaOf(centerTile).get
}
