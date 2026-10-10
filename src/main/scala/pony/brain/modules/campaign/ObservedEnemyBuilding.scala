package pony
package brain
package modules
package campaign

import pony.geometry.MapTilePosition

case class ObservedEnemyBuilding(id: Int, tile: MapTilePosition, width: Int, height: Int, base: Boolean) {
  def footprint = for (x <- tile.x until tile.x + width; y <- tile.y until tile.y + height)
    yield MapTilePosition(x, y)
}
