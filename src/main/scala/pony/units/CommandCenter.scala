package pony
package units

import pony.geometry.{Area, MapTilePosition}

import bwapi.{Unit => APIUnit, _}

class CommandCenter(unit: APIUnit)
    extends AnyUnit(unit) with MainBuilding with CanBuildAddons with TerranBuilding {
  var relocating                                                = false
  override def canBuild[T <: Mobile](typeOfUnit: Class[? <: T]) =
    !relocating && !isFloating && super.canBuild(typeOfUnit)

  // A depot about to fly off would leave its add-on behind, and the add-on job would keep it from lifting. So would one
  // that shares its field with another, built at home to fly to its own (TerranEconomicOpening): in game 2 vs Terran on
  // Lost_Temple at a52f427 three of them got a comsat just before they flew off, from frame 30000 on we mostly had
  // none, and no sweep looked at the island where the enemy's last base stood for forty thousand frames.
  override def mayTakeAddon = !relocating && !isFloating && !sharesField && super.mayTakeAddon

  /** Shares its field with another landed depot, and is not the home depot, which stays. */
  private def sharesField = {
    val all = bases.allBases
    !bases.mainBase.exists(_.mainBuilding == this) &&
    all.find(_.mainBuilding == this).flatMap(_.resourceArea).exists { field =>
      all.exists(b => b.mainBuilding != this && !b.mainBuilding.isFloating && b.resourceArea.contains(field))
    }
  }
  // Other buildings are static; this depot deliberately changes its resource field after lifting.
  override def tilePosition = {
    val p = nativeUnit.getTilePosition
    MapTilePosition.shared(p.getX, p.getY)
  }
  override def area      = Area(tilePosition, size)
  override def areaOnMap = mapLayers.rawWalkableMap.areaOf(centerTile).get
}
