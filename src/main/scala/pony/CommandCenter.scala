package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

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
