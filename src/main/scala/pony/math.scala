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
  val asTuple       = (x, y)
  val asMapPosition = MapPosition(x * tileSize, y * tileSize)
  def randomized(shuffle: Int) = {
    val xRand = math.random * shuffle - shuffle * 0.5
    val yRand = math.random * shuffle - shuffle * 0.5
    movedBy(xRand.toInt, yRand.toInt)
  }
  def movedBy(offX: Int, offY: Int) = MapTilePosition.shared(x + offX, y + offY)
  def asTilePosition = new TilePosition(x, y)
  def asNative = MapTilePosition.nativeShared(x, y)
  def mapX = tileSize * x
  def mapY = tileSize * y
  def movedBy(other: HasXY) = MapTilePosition.shared(x + other.x, y + other.y)
  override def toString = s"($x,$y)"
}
object MapTilePosition {
  val max          = 256 * 4
  val points       = Array.tabulate(max * 2, max * 2)((x, y) => MapTilePosition(x - max, y - max))
  val nativePoints = Array.tabulate(max * 2, max * 2)((x, y) => new Position(x - max, y - max))
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
    if (inRange(x, y))
      points(x + max)(y + max)
    else {
      strange.computeIfAbsent((x, y), computer)
    }
  private def inRange(x: Int, y: Int) = x > -max && y > -max && x < max && y < max
  def nativeShared(xy: (Int, Int)): Position = nativeShared(xy._1, xy._2)
  def nativeShared(x: Int, y: Int): Position =
    if (inRange(x, y))
      nativePoints(x + max)(y + max)
    else {
      nativeStrange.computeIfAbsent((x, y), nativeComputer)
    }
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

case class Line(a: MapTilePosition, b: MapTilePosition) {
  val length = a.distanceTo(b)
  val center = MapTilePosition.shared((a.x + b.x) / 2, (a.y + b.y) / 2)
  def movedBy(center: HasXY) = {
    Line(a.movedBy(center), b.movedBy(center))
  }
}

case class Area(upperLeft: MapTilePosition, sizeOfArea: Size) {

  val lowerRight = upperLeft.movedBy(sizeOfArea)
  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2, (upperLeft.mapY + lowerRight.mapY) / 2)
  def distanceTo(tilePosition: MapTilePosition) = {
    // TODO optimize
    outline.minBy(_.distanceToSquared(tilePosition)).distanceTo(tilePosition)
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
  def distanceTo(area: Area) = {
    closestDirectConnection(area).length
  }
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

class MultiArea(areas: Seq[Area])

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