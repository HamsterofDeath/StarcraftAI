package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait TerranBuilding extends Building {
  private val myCurrentArea = oncePer(Primes.prime59) {
    mapLayers.rawWalkableMap.areaOf(centerTile).orElse {
      mapLayers.rawWalkableMap
      .spiralAround(centerTile, 5)
      .map(mapLayers.rawWalkableMap.areaOf)
      .find(_.isDefined)
      .map(_.get)
    }
  }

  def currentAreaOnMap = myCurrentArea.get

  /** A lifted building moves; always read the live native position, never the static cache. */
  override def tilePosition = {
    val position = nativeUnit.getTilePosition
    MapTilePosition.shared(position.getX, position.getY)
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!isFloating && super.tilePosition != tilePosition) {
      refreshPositionCaches()
    }
  }

}
