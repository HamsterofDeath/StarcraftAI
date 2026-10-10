package pony
package geometry

case class Line(a: MapTilePosition, b: MapTilePosition) {
  val length = a.distanceTo(b)
  val center = MapTilePosition.shared((a.x + b.x) / 2, (a.y + b.y) / 2)

  def movedBy(center: HasXY) = {
    Line(a.movedBy(center), b.movedBy(center))
  }

  def split = Line(a, center) -> Line(center, b)
}
