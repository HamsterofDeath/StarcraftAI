package pony
package terrain

import pony.geometry.{Area, Grid2D}

private[pony] object ResourceDepotPlacement {
  def permitted(area: Area, resourceDepot: Boolean, resourceBuffer: Grid2D): Boolean =
    !resourceDepot || resourceBuffer.free(area)
}
