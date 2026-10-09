package pony

import java.util.concurrent.ConcurrentHashMap
import java.util.function.Function

import bwapi.{Position, TilePosition}

case class MapTilePosition(x: Int, y: Int) extends HasXY {
  def isInsideOfGame = !isOutsideOfGame

  def isOutsideOfGame = x > 1000 && y > 1000

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
    val xRand = math.random() * shuffle - shuffle * 0.5
    val yRand = math.random() * shuffle - shuffle * 0.5
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

  val max    = 256 * 4
  val points = {
    if (memoryHog) {
      Array.tabulate(max * 2, max * 2)((x, y) => MapTilePosition(x - max, y - max))
    } else { Array.empty[Array[MapTilePosition]] }
  }
  val nativePoints = {
    if (memoryHog) {
      Array.tabulate(max * 2, max * 2)((x, y) => new Position(x - max, y - max))
    } else { Array.empty[Array[Position]] }
  }
  val zero                  = MapTilePosition.shared(0, 0)
  private val strange       = new ConcurrentHashMap[(Int, Int), MapTilePosition]
  private val nativeStrange = new ConcurrentHashMap[(Int, Int), Position]
  private val computer      = new Function[(Int, Int), MapTilePosition] {
    override def apply(t: (Int, Int)) = MapTilePosition(t._1, t._2)
  }
  private val nativeComputer = new Function[(Int, Int), Position] {
    override def apply(t: (Int, Int)) = new Position(t._1, t._2)
  }

  def averageOpt(ps: IterableOnce[MapTilePosition]) = {
    if (ps.iterator.isEmpty) None else Some(average(ps))
  }

  def average(ps: IterableOnce[MapTilePosition]) = {
    var size = 0
    ps.iterator.foldLeft(MapTilePosition.zero)((acc, e) => {
      size += 1
      acc.movedBy(e)
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
