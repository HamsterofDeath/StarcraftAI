package pony
package units

import pony.geometry.MapTilePosition

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
