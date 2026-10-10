package pony
package brain
package modules
package wall

import scala.collection.mutable

/**
  * Pixel-accurate passability for walls. Buildings block with their collision boxes, which leave gaps of up to 16
  * pixels inside their tiles; whether a unit slips between two buildings depends on those gaps and on the unit's own
  * box. Pure, so walls can be checked without a game.
  */
private[pony] object WallGeometry {

  /** A collision box in pixels, inclusive on all sides. */
  final case class Box(left: Int, top: Int, right: Int, bottom: Int) {
    def intersects(o: Box): Boolean = left <= o.right && o.left <= right && top <= o.bottom && o.top <= bottom
  }

  /** A unit type's tile footprint and its collision box around its center (BWAPI dimensionLeft/Up/Right/Down). */
  final case class Dims(tileWidth: Int, tileHeight: Int, left: Int, up: Int, right: Int, down: Int)

  object Dims {
    def of(t: bwapi.UnitType): Dims =
      Dims(t.tileWidth, t.tileHeight, t.dimensionLeft, t.dimensionUp, t.dimensionRight, t.dimensionDown)

    val SupplyDepot = Dims(3, 2, 38, 22, 38, 26)
    val Barracks    = Dims(4, 3, 48, 40, 56, 32)
    val Zealot      = Dims(1, 1, 11, 5, 11, 13)
    val Marine      = Dims(1, 1, 8, 9, 8, 10)
    val SiegeTank   = Dims(1, 1, 16, 16, 15, 15)
  }

  /** The collision box of a building whose upper left tile is (tileX, tileY). */
  def buildingBox(tileX: Int, tileY: Int, kind: Dims): Box = {
    val cx = tileX * 32 + kind.tileWidth * 16
    val cy = tileY * 32 + kind.tileHeight * 16
    Box(cx - kind.left, cy - kind.up, cx + kind.right, cy + kind.down)
  }

  /**
    * Whether a unit can move from one of `from` (pixel centers) to a center for which `inside` holds, staying within
    * `region`, never overlapping an obstacle box nor a blocked walk tile (8x8 pixels). Centers are searched every `step`
    * pixels, so gaps are judged to within that many pixels.
    */
  def passable(
      unit: Dims,
      obstacles: Seq[Box],
      blockedWalkTile: (Int, Int) => Boolean,
      region: Box,
      from: Seq[(Int, Int)],
      inside: (Int, Int) => Boolean,
      step: Int = 4
  ): Boolean = path(unit, obstacles, blockedWalkTile, region, from, inside, step).isDefined

  /** The centers of one shortest way through, as `passable` judges it, from a start to the inside. */
  def path(
      unit: Dims,
      obstacles: Seq[Box],
      blockedWalkTile: (Int, Int) => Boolean,
      region: Box,
      from: Seq[(Int, Int)],
      inside: (Int, Int) => Boolean,
      step: Int = 4
  ): Option[Vector[(Int, Int)]] = {
    def free(cx: Int, cy: Int): Boolean = {
      val body = Box(cx - unit.left, cy - unit.up, cx + unit.right, cy + unit.down)
      body.left >= region.left && body.top >= region.top && body.right <= region.right &&
      body.bottom <= region.bottom && !obstacles.exists(_.intersects(body)) && {
        val walkTiles = for {
          wx <- (body.left / 8) to (body.right / 8)
          wy <- (body.top / 8) to (body.bottom / 8)
        } yield (wx, wy)
        !walkTiles.exists(blockedWalkTile.tupled)
      }
    }
    def snap(v: Int) = v - Math.floorMod(v, step)
    val parent       = mutable.HashMap.empty[(Int, Int), (Int, Int)]
    val visited      = mutable.HashSet.empty[(Int, Int)]
    val queue        = mutable.Queue.empty[(Int, Int)]
    from.map((x, y) => (snap(x), snap(y))).filter(free.tupled).foreach { p =>
      if (visited.add(p)) queue += p
    }
    var reached = Option.empty[(Int, Int)]
    while (queue.nonEmpty && reached.isEmpty) {
      val (x, y) = queue.dequeue()
      if (inside(x, y)) reached = Some((x, y))
      else for ((dx, dy) <- Seq((step, 0), (-step, 0), (0, step), (0, -step))) {
        val next = (x + dx, y + dy)
        if (!visited(next) && free(next._1, next._2)) {
          visited += next
          parent(next) = (x, y)
          queue += next
        }
      }
    }
    reached.map { end =>
      Iterator.iterate(Option(end))(_.flatMap(parent.get)).takeWhile(_.isDefined).flatten.toVector.reverse
    }
  }
}
