package pony

import bwapi.Position

trait HasXY {
  def x: Int
  def y: Int

  def distanceTo(other: HasXY) = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    math.sqrt(xDiff * xDiff + yDiff * yDiff)
  }

}
case class MapTilePosition(x: Int, y: Int) extends HasXY {
  def mapX = tileSize * x
  def mapY = tileSize * y

  def toNative = new Position(x, y)

  def toMapPosition = MapPosition(x * tileSize, y * tileSize)

  def movedBy(other: HasXY) = MapTilePosition.shared(x + other.x, y + other.y)

  override def toString = s"($x,$y)"
}
object MapTilePosition {
  val points = Array.tabulate(256 * 4, 256 * 4)((x, y) => MapTilePosition(x, y))
  def shared(x: Int, y: Int) = points(x)(y)
}

case class Size(x: Int, y: Int) extends HasXY
object Size {
  val sizes = Array.tabulate(5, 5)((x, y) => Size(x, y))
  def shared(x: Int, y: Int) = sizes(x)(y)
}

case class Area(upperLeft: MapTilePosition, sizeOfArea: Size) {

  val lowerRight = upperLeft.movedBy(sizeOfArea)
  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2, (upperLeft.mapY + lowerRight.mapY) / 2)
  def outline: Traversable[MapTilePosition] = {
    new Traversable[MapTilePosition] {
      override def foreach[U](f: (MapTilePosition) => U): Unit = {
        (0 until sizeOfArea.x).foreach { x =>
          f(MapTilePosition.shared(upperLeft.x + x, upperLeft.y))
          f(MapTilePosition.shared(upperLeft.x + x, upperLeft.y + sizeOfArea.y))
        }
        (1 until sizeOfArea.y-1).foreach { y =>
          f(MapTilePosition.shared(upperLeft.x, upperLeft.y + y))
          f(MapTilePosition.shared(upperLeft.x + sizeOfArea.x, upperLeft.y + y))
        }
      }
    }

  }
  def describe = s"$upperLeft/$lowerRight"
}

case class MapPosition(x: Int, y: Int) extends HasXY {
  def toNative = new Position(x, y)
}