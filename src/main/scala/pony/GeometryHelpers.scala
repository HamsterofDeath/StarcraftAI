package pony

import scala.collection.mutable

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
                           blockSize: Int = 45): Iterable[MapTilePosition] = new
      Iterable[MapTilePosition] {
    override def iterator: Iterator[MapTilePosition] =
      iterateBlockSpiralClockWise(origin, blockSize)
      .map(e => MapTilePosition.shared(e.x, e.y))
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
    def tilesInCircle(seq: TraversableOnce[MapTilePosition], minRange: Int, times: Int) = {
      tilesInCircleWithRange(seq.map(_ -> 0), minRange, times)
    }

    def tilesInCircleWithRange(seq: TraversableOnce[(MapTilePosition, Int)], minRange: Int,
                               times: Int) = {
      val counts = mutable.HashMap.empty[MapTilePosition, Int]
      seq.foreach { p =>
        self.tilesInCircle(p._1, minRange max p._2).foreach { where =>
          counts.insertReplace(where, _ + 1, 1)
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
