package pony

import java.util.concurrent.ConcurrentHashMap
import java.util.function.Function

import bwapi.{Position, TilePosition}

import scala.collection.mutable

trait HasXY {
  def x: Int
  def y: Int

  def distanceToIsLess(other: HasXY, dst: Int) = distanceSquaredTo(other) < dst * dst

  def distanceToIsMore(other: HasXY, dst: Int) = distanceSquaredTo(other) > dst * dst

  def distanceSquaredTo(other: HasXY) = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    xDiff * xDiff + yDiff * yDiff
  }

  def distanceTo(other: HasXY) = math.sqrt(distanceSquaredTo(other))

}

case class MapTilePosition(x: Int, y: Int) extends HasXY {
  def asTuple = (x, y)

  def middleBetween(center: MapTilePosition) = {
    movedBy(center) / 2
  }

  def /(i: Int) = MapTilePosition.shared(x / i, y / i)

  def movedBy(other: HasXY) = MapTilePosition.shared(x + other.x, y + other.y)

  def isAtStorePosition = x >= 1000 && y >= 1000

  def diffTo(other: MapTilePosition) = {
    MapTilePosition.shared(other.x - x, other.y - y)
  }

  def leftRightUpDown = (movedBy(-1, 0), movedBy(1, 0), movedBy(0, -1), movedBy(0, 1))

  def movedBy(offX: Int, offY: Int) = MapTilePosition.shared(x + offX, y + offY)

  def asArea = Area(this, this)

  def nativeMapPosition = asMapPosition.toNative

  def asMapPosition = MapPosition(x * tileSize, y * tileSize)

  def randomized(shuffle: Int) = {
    val xRand = math.random * shuffle - shuffle * 0.5
    val yRand = math.random * shuffle - shuffle * 0.5
    movedBy(xRand.toInt, yRand.toInt)
  }

  def asTilePosition = new TilePosition(x, y)

  def asNative = MapTilePosition.nativeShared(x, y)

  def mapX = tileSize * x

  def mapY = tileSize * y

  def movedByNew(other: HasXY) = MapTilePosition(x + other.x, y + other.y)

  override def toString = s"($x,$y)"
}

object MapTilePosition {

  val max          = 256 * 4
  val points       = {
    if (memoryHog) {
      Array.tabulate(max * 2, max * 2)((x, y) => MapTilePosition(x - max, y - max))
    } else {Array.empty[Array[MapTilePosition]]}
  }
  val nativePoints = {
    if (memoryHog) {
      Array.tabulate(max * 2, max * 2)((x, y) => new Position(x - max, y - max))
    } else {Array.empty[Array[Position]]}
  }
  val zero         = MapTilePosition.shared(0, 0)
  private val strange        = new ConcurrentHashMap[(Int, Int), MapTilePosition]
  private val nativeStrange  = new ConcurrentHashMap[(Int, Int), Position]
  private val computer       = new Function[(Int, Int), MapTilePosition] {
    override def apply(t: (Int, Int)) = MapTilePosition(t._1, t._2)
  }
  private val nativeComputer = new Function[(Int, Int), Position] {
    override def apply(t: (Int, Int)) = new Position(t._1, t._2)
  }

  def averageOpt(ps: TraversableOnce[MapTilePosition]) = {
    if (ps.isEmpty) None else Some(average(ps))
  }

  def average(ps: TraversableOnce[MapTilePosition]) = {
    var size = 0
    ps.foldLeft(MapTilePosition.zero)((acc, e) => {
      size += 1
      acc movedBy e
    }) / size
  }

  def shared(xy: (Int, Int)): MapTilePosition = shared(xy._1, xy._2)

  def shared(x: Int, y: Int): MapTilePosition = {
    if (memoryHog) {
      if (inRange(x, y))
        points(x + max)(y + max)
      else {
        strange.computeIfAbsent((x, y), computer)
      }
    } else {
      MapTilePosition(x, y)
    }
  }

  def nativeShared(xy: (Int, Int)): Position = nativeShared(xy._1, xy._2)

  def nativeShared(x: Int, y: Int): Position = {
    if (memoryHog) {
      if (inRange(x, y))
        nativePoints(x + max)(y + max)
      else {
        nativeStrange.computeIfAbsent((x, y), nativeComputer)
      }
    } else {
      new Position(x, y)
    }
  }

  private def inRange(x: Int, y: Int) = x > -max && y > -max && x < max && y < max

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
  val sizes = if (memoryHog) {Array.tabulate(20, 20)((x, y) => Size(x, y))} else {
    Array.empty[Array[Size]]
  }

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
  def height = sizeOfArea.y

  def width = sizeOfArea.x

  val lowerRight = upperLeft.movedBy(sizeOfArea).movedBy(-1, -1)
  val edges      = upperLeft ::
                   MapTilePosition(lowerRight.x, upperLeft.y) ::
                   lowerRight ::
                   MapTilePosition(upperLeft.x, lowerRight.y) ::
                   Nil
  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2,
    (upperLeft.mapY + lowerRight.mapY) / 2)
  val centerTile = MapTilePosition((upperLeft.x + lowerRight.x) / 2, (upperLeft.y + lowerRight.y)
                                                                     / 2)

  def moveTo(e: MapTilePosition) = copy(upperLeft = e)

  def extendedBy(tiles: Int) = {
    growBy(tiles)
  }

  def growBy(tiles: Int) = {
    tiles match {
      case 0 => this
      case n => Area(upperLeft.movedBy(-n, -n), lowerRight.movedBy(n, n))
    }
  }

  def distanceTo(tilePosition: MapTilePosition) = {
    // TODO optimize
    outline.minBy(_.distanceSquaredTo(tilePosition)).distanceTo(tilePosition)
  }

  def distanceTo(area: Area) = {
    closestDirectConnection(area).length
  }

  def closestDirectConnection(area: Area): Line = {
    // TODO optimize
    val from = outline.minBy { p =>
      area.outline.minBy(_.distanceSquaredTo(p)).distanceTo(p)
    }

    val to = area.outline.minBy { p =>
      outline.minBy(_.distanceSquaredTo(p)).distanceTo(p)
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

  def anyTile = upperLeft

  def closestDirectConnection(elem: StaticallyPositioned): Line =
    closestDirectConnection(elem.area)

  def tiles: Traversable[MapTilePosition] = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      sizeOfArea.points.map { p => f(p.movedBy(upperLeft)) }
    }
  }

  def describe = s"$upperLeft/$lowerRight"
}

object Area {
  def apply(upperLeft: MapTilePosition, lowerRight: MapTilePosition): Area = {
    Area(upperLeft, Size.shared(lowerRight.x - upperLeft.x + 1, lowerRight.y - upperLeft.y + 1))
  }
}

class MultiArea(areas: Seq[Area])

case class MapPosition(x: Int, y: Int) extends HasXY {
  def toNative = new Position(x, y)
}

class GeometryHelpers(maxX: Int, maxY: Int) {
  self =>

  def circle(center: MapTilePosition, r: Int) = Circle(center, r, maxX, maxY)

  def tilesInCircle(position: MapTilePosition, radius: Int) = {
    val fromX = 0 max position.x - radius
    val toX = maxX min position.x + radius
    val fromY = 0 max position.y - radius
    val toY = maxY min position.y + radius
    val radSqr = radius * radius
    val x2 = position.x
    val y2 = position.y
    def dstSqr(x: Int, y: Int) = {
      val xx = x - x2
      val yy = y - y2
      xx * xx + yy * yy
    }

    val xSize = toX - fromX
    val ySize = toY - fromY
    val tiles = xSize * ySize

    Iterator.range(fromX, toX + 1).map { x =>
      Iterator.range(fromY, toY + 1).filter { y =>
        dstSqr(x, y) <= radSqr
      }.map { y => MapTilePosition.shared(x, y) }
    }.flatten
  }

  def blockSpiralClockWise(origin: MapTilePosition,
                           blockSize: Int = 45): Traversable[MapTilePosition] = new
      Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      iterateBlockSpiralClockWise(origin, blockSize)
      .foreach(e => f(MapTilePosition.shared(e.x, e.y)))
    }
  }

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

    val maxDst = blockSize * blockSize
    Iterator.iterate(new MutableXY)(_.next_!())
    .take(blockSize * blockSize)
    .filter(e => e.x >= 0 && e.x < maxX && e.y >= 0 && e.y < maxY)
    .filter { e =>
      val a = e.x - origin.x
      val b = e.y - origin.y
      a * a + b * b <= maxDst
    }
    .map(mut => MapTilePosition.shared(mut.x, mut.y))
  }

  object intersections {
    def tilesInCircle(seq: TraversableOnce[MapTilePosition], range: Int, times: Int) = {
      val counts = mutable.HashMap.empty[MapTilePosition, Int]
      seq.foreach { p =>
        self.tilesInCircle(p, range).foreach { where =>
          counts.insertReplace(where, _ + 1, 0)
        }
      }

      counts.iterator.filter { case (_, count) =>
        count >= times
      }.map(_._1)
    }

    def tilesInSquare(seq: TraversableOnce[MapTilePosition], range: Int, times: Int) = {
      val counts = mutable.HashMap.empty[MapTilePosition, Int]
      seq.foreach { p =>
        iterateBlockSpiralClockWise(p, range * 2).foreach { where =>
          counts.insertReplace(where, _ + 1, 0)
        }
      }

      counts.iterator.filter { case (_, count) =>
        count >= times
      }.map(_._1)
    }
  }
}
