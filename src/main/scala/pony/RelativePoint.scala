package pony

case class RelativePoint(xOff: Int, yOff: Int) extends HasXY {
  lazy val opposite = RelativePoint(-xOff, -yOff)

  def asMapTile = MapTilePosition.shared(x, y)

  override def x = xOff

  override def y = yOff
}
