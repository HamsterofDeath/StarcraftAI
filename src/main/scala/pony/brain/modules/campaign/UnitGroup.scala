package pony
package brain
package modules
package campaign

import pony.geometry.MapTilePosition
import pony.units.WrapsUnit

case class UnitGroup[T <: WrapsUnit](members: Seq[T], center: MapTilePosition)
