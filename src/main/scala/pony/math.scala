package pony

import java.util.concurrent.ConcurrentHashMap
import java.util.function.Function

import bwapi.{Position, TilePosition}

trait HasXY {
  def x: Int
  def y: Int

  def distanceTo(other: HasXY) = math.sqrt(distanceToSquared(other))

  def distanceToSquared(other: HasXY) = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    xDiff * xDiff + yDiff * yDiff
  }

}
case class MapTilePosition(x: Int, y: Int) extends HasXY {
  def middleBetween(center: MapTilePosition) = {
    movedBy(center) / 2
  }

  def isAtStorePosition = x >= 1000 && y >= 1000

  def diffTo(other: MapTilePosition) = {
    MapTilePosition.shared(other.x - x, other.y - y)
  }

  def leftRightUpDown = (movedBy(-1, 0), movedBy(1, 0), movedBy(0, -1), movedBy(0, 1))

  def asArea = Area(this, this)

  val asTuple       = (x, y)
  val asMapPosition = MapPosition(x * tileSize, y * tileSize)
  def nativeMapPosition = asMapPosition.toNative
  def randomized(shuffle: Int) = {
    val xRand = math.random * shuffle - shuffle * 0.5
    val yRand = math.random * shuffle - shuffle * 0.5
    movedBy(xRand.toInt, yRand.toInt)
  }
  def movedBy(offX: Int, offY: Int) = MapTilePosition.shared(x + offX, y + offY)
  def /(i: Int) = MapTilePosition.shared(x / i, y / i)
  def asTilePosition = new TilePosition(x, y)
  def asNative = MapTilePosition.nativeShared(x, y)
  def mapX = tileSize * x
  def mapY = tileSize * y
  def movedBy(other: HasXY) = MapTilePosition.shared(x + other.x, y + other.y)
  def movedByNew(other: HasXY) = MapTilePosition(x + other.x, y + other.y)
  override def toString = s"($x,$y)"
}
object MapTilePosition {
  val max          = 256 * 4
  val points       = if (memoryHog) {
    Array.tabulate(max * 2, max * 2)((x, y) => MapTilePosition(x - max, y - max))
  } else {Array.empty[Array[MapTilePosition]]}
  val nativePoints = if (memoryHog) {
    Array.tabulate(max * 2, max * 2)((x, y) => new Position(x - max, y - max))
  } else {Array.empty[Array[Position]]}
  private val strange       = new ConcurrentHashMap[(Int, Int), MapTilePosition]
  private val nativeStrange = new ConcurrentHashMap[(Int, Int), Position]

  private val computer       = new Function[(Int, Int), MapTilePosition] {
    override def apply(t: (Int, Int)) = MapTilePosition(t._1, t._2)
  }

  private val nativeComputer = new Function[(Int, Int), Position] {
    override def apply(t: (Int, Int)) = new Position(t._1, t._2)
  }
  def shared(xy: (Int, Int)): MapTilePosition = shared(xy._1, xy._2)

  def shared(x: Int, y: Int): MapTilePosition =
    if (memoryHog) {
      if (inRange(x, y))
        points(x + max)(y + max)
      else {
        strange.computeIfAbsent((x, y), computer)
      }
    } else {
      MapTilePosition(x, y)
    }

  private def inRange(x: Int, y: Int) = x > -max && y > -max && x < max && y < max
  def nativeShared(xy: (Int, Int)): Position = nativeShared(xy._1, xy._2)
  def nativeShared(x: Int, y: Int): Position =
    if (memoryHog) {
      if (inRange(x, y))
        nativePoints(x + max)(y + max)
      else {
        nativeStrange.computeIfAbsent((x, y), nativeComputer)
      }
    } else {
      new Position(x, y)
    }

  val zero = MapTilePosition.shared(0, 0)

}

case class Size(x: Int, y: Int) extends HasXY {
  def growBy(i: Int) = Size.shared(x + i, y + i)

  def points: Traversable[MapTilePosition] = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      for (x <- 0 until x; y <- 0 until y) {
        f(MapTilePosition.shared(x, y))
      }
    }
  }
}

object Size {
  val sizes = if (memoryHog) {Array.tabulate(20, 20)((x, y) => Size(x, y))} else {Array.empty[Array[Size]]}
  def shared(x: Int, y: Int) = if (memoryHog) {sizes(x)(y)} else {Size(x, y)}
}

case class Line(a: MapTilePosition, b: MapTilePosition) {
  val length = a.distanceTo(b)
  val center = MapTilePosition.shared((a.x + b.x) / 2, (a.y + b.y) / 2)
  def movedBy(center: HasXY) = {
    Line(a.movedBy(center), b.movedBy(center))
  }
  def split = Line(a, center) -> Line(center, b)
}


case class Area(upperLeft: MapTilePosition, sizeOfArea: Size) {
  def moveTo(e: MapTilePosition) = copy(upperLeft = e)

  def growBy(tiles: Int) = {
    tiles match {
      case 0 => this
      case n => Area(upperLeft.movedBy(-n, -n), lowerRight.movedBy(n, n))
    }
  }

  def extendedBy(tiles: Int) = {
    growBy(tiles)
  }

  val lowerRight = upperLeft.movedBy(sizeOfArea).movedBy(-1, -1)

  val edges = upperLeft ::
              MapTilePosition(lowerRight.x, upperLeft.y) ::
              lowerRight ::
              MapTilePosition(upperLeft.x, lowerRight.y) ::
              Nil

  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2, (upperLeft.mapY + lowerRight.mapY) / 2)
  val centerTile = MapTilePosition((upperLeft.x + lowerRight.x) / 2, (upperLeft.y + lowerRight.y) / 2)
  def distanceTo(tilePosition: MapTilePosition) = {
    // TODO optimize
    outline.minBy(_.distanceToSquared(tilePosition)).distanceTo(tilePosition)
  }
  def distanceTo(area: Area) = {
    closestDirectConnection(area).length
  }
  def anyTile = upperLeft
  def closestDirectConnection(elem: StaticallyPositioned): Line =
    closestDirectConnection(elem.area)
  def closestDirectConnection(area: Area): Line = {
    // TODO optimize
    val from = outline.minBy { p =>
      area.outline.minBy(_.distanceToSquared(p)).distanceTo(p)
    }

    val to = area.outline.minBy { p =>
      outline.minBy(_.distanceToSquared(p)).distanceTo(p)
    }
    Line(from, to)

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
      override def isEmpty = false
    }

  }
  def tiles: Traversable[MapTilePosition] = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      sizeOfArea.points.map { p => f(p.movedBy(upperLeft)) }
    }
  }
  def describe = s"$upperLeft/$lowerRight"
}

object Area {
  def apply(upperLeft:MapTilePosition, lowerRight:MapTilePosition):Area = {
    Area(upperLeft, Size.shared(lowerRight.x - upperLeft.x + 1, lowerRight.y - upperLeft.y + 1))
  }
}


class MultiArea(areas: Seq[Area])

case class MapPosition(x: Int, y: Int) extends HasXY {
  def toNative = new Position(x, y)
}

class GeometryHelpers(maxX: Int, maxY: Int) {

  def iterateBlockSpiralClockWise(origin: MapTilePosition, blockSize: Int = 45) = {
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
    .map(mut => MapTilePosition.shared(mut.x, mut.y))
  }

  def blockSpiralClockWise(origin: MapTilePosition, blockSize: Int = 45): Traversable[MapTilePosition] = new
      Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      iterateBlockSpiralClockWise(origin, blockSize).foreach(e => f(MapTilePosition.shared(e.x, e.y)))
    }
  }
}
