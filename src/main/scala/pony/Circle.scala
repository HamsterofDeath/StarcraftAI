package pony

case class Circle(center: MapTilePosition, radius: Int, maxX: Int, maxY: Int) {
  def asTiles = new GeometryHelpers(maxX, maxY).tilesInCircle(center, radius)
}
