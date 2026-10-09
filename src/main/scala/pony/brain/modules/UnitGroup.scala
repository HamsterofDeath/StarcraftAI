package pony
package brain
package modules

case class UnitGroup[T <: WrapsUnit](members: Seq[T], center: MapTilePosition)
