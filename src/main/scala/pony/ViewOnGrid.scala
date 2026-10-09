package pony

import scala.reflect.ClassTag

class ViewOnGrid(ug: UnitGrid, hostile: Boolean) {
  def onTile(tile: MapTilePosition) = ug.onTile(tile, hostile)

  def allInRange[T <: Mobile: ClassTag](tile: MapTilePosition, radius: Int) = ug.allInRangeOf[T](
    tile,
    radius,
    !hostile
  )
}
