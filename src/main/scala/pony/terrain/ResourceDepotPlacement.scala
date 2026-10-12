package pony
package terrain

import pony.geometry.{Area, Grid2D, MapTilePosition}

private[pony] object ResourceDepotPlacement {
  def permitted(area: Area, resourceDepot: Boolean, resourceBuffer: Grid2D): Boolean =
    !resourceDepot || resourceBuffer.free(area)

  /**
    * Whether a depot on this footprint serves the field it was placed for: a base binds to the nearest field and mines
    * it only within reach. In game 5 vs Protoss on 09c3bb2 our own buildings covered the natural's site, the finder took
    * a spot 20 tiles off that bound to the next field, and two depots shared one mineral line while the natural stayed
    * empty.
    */
  def bindsTo(depot: Area, field: MapTilePosition, otherFields: Iterable[MapTilePosition], reach: Int): Boolean = {
    val own = depot.distanceTo(field)
    !depot.centerTile.distanceToIsMore(field, reach) && otherFields.forall(o => depot.distanceTo(o) > own)
  }
}
