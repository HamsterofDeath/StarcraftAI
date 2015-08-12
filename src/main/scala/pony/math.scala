package pony

import bwapi.{Position, TilePosition}

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
  val asTuple = (x, y)

  val asTilePosition = new TilePosition(x, y)
  val asNative       = new Position(x, y)
  val asMapPosition  = MapPosition(x * tileSize, y * tileSize)
  def mapX = tileSize * x
  def mapY = tileSize * y
  def movedBy(other: HasXY) = MapTilePosition.shared(x + other.x, y + other.y)
  def movedBy(offX: Int, offY: Int) = MapTilePosition.shared(x + offX, y + offY)

  override def toString = s"($x,$y)"
}
object MapTilePosition {
  val max    = 256 * 4
  val points = Array.tabulate(max, max)((x, y) => MapTilePosition(x, y))
  def shared(xy: (Int, Int)): MapTilePosition = shared(xy._1, xy._2)
  def shared(x: Int, y: Int): MapTilePosition = points(x)(y)
}

case class Size(x: Int, y: Int) extends HasXY {
  def points: Traversable[MapTilePosition] = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      for (x <- 0 until x; y <- 0 until y) {
        f(MapTilePosition.shared(x, y))
      }
    }
  }
}

object Size {
  val sizes = Array.tabulate(5, 5)((x, y) => Size(x, y))
  def shared(x: Int, y: Int) = sizes(x)(y)
}

case class Area(upperLeft: MapTilePosition, sizeOfArea: Size) {
  val lowerRight = upperLeft.movedBy(sizeOfArea)
  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2, (upperLeft.mapY + lowerRight.mapY) / 2)
  def closestDirectConnection(elem: StaticallyPositioned) = {
    // TODO optimize
    val from = outline.minBy { p =>
      elem.area.outline.minBy(_.distanceTo(p)).distanceTo(p)
    }

    val to = elem.area.outline.minBy { p =>
      outline.minBy(_.distanceTo(p)).distanceTo(p)
    }
    from -> to
  }
  def outline: Traversable[MapTilePosition] = {
    new Traversable[MapTilePosition] {
      override def foreach[U](f: (MapTilePosition) => U): Unit = {
        (0 until sizeOfArea.x).foreach { x =>
          f(MapTilePosition.shared(upperLeft.x + x, upperLeft.y))
          f(MapTilePosition.shared(upperLeft.x + x, upperLeft.y + sizeOfArea.y))
        }
        (1 until sizeOfArea.y - 1).foreach { y =>
          f(MapTilePosition.shared(upperLeft.x, upperLeft.y + y))
          f(MapTilePosition.shared(upperLeft.x + sizeOfArea.x, upperLeft.y + y))
        }
      }
    }

  }
  def tiles: Traversable[MapTilePosition] = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      sizeOfArea.points.map { p => f(p.movedBy(upperLeft)) }
    }
  }
  def describe = s"$upperLeft/$lowerRight"
}

case class MapPosition(x: Int, y: Int) extends HasXY {
  def toNative = new Position(x, y)
}

class GeometryHelpers(maxX: Int, maxY: Int) {

  def blockSpiralClockWise(origin: MapTilePosition, blockSize: Int = 45): Traversable[MapTilePosition] = new
      Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      val directions = {
        class MutableXY {
          private var turns                       = 0
          private var remainingInCurrentDirection = 1
          private var myX                         = origin.x
          private var myY                         = origin.y

          def next_!(): MutableXY = {
            if (remainingInCurrentDirection > 0) {
              remainingInCurrentDirection -= 1
              if (remainingInCurrentDirection == 0) {
                turns += 1
                remainingInCurrentDirection = if (turns % 2 == 0) turns / 2 else (turns + 1) / 2
              }
            }
            (turns - 1) % 4 match {
              case 0 => myX += 1
              case 1 => myY += 1
              case 2 => myX -= 1
              case 3 => myY -= 1
            }
            this
          }

          def x = myX
          def y = myY
        }

        Iterator.iterate(new MutableXY)(_.next_!())
        .take(blockSize * blockSize)
        .filter(e => e.x >= 0 && e.x < maxX && e.y >= 0 && e.y < maxY)
        .foreach(e => f(MapTilePosition.shared(e.x, e.y)))
      }
    }
  }
}