package pony
package pathing

import pony.geometry.MapTilePosition

/**
  * An army marching in formation, kept free of the game. A front moves along the route while the army keeps up, and
  * every unit's slot lies behind the front by how much shorter its weapon reaches than the longest: the long range
  * leads, every weapon comes into range together, and the army arrives as one instead of strung out by speed. Units of
  * one range stand side by side across the route, a row at most RowWidth wide, further rows RowDepth behind.
  */
private[pony] object Formation {

  /** Tiles the front moves at a time... */
  val StepTiles = 2.0

  /** ...once this share of the army stands within CohesionTiles of its slot... */
  val KeepUpShare   = 0.8
  val CohesionTiles = 3.0

  /** ...or once it has waited this long: a unit that cannot reach its slot must not hold the army forever. */
  val StallFrames = 24 * 20

  val RowWidth = 7
  val RowDepth = 1.5

  type Point = (Double, Double)

  /** A route measured in tiles along its length. */
  final class Route(points: Seq[MapTilePosition]) {
    private val corners = points.map(p => (p.x.toDouble, p.y.toDouble)).toVector
    private val along   = corners.zip(corners.drop(1)).scanLeft(0.0)((sum, ab) => sum + distance(ab._1, ab._2))

    val length = along.lastOption.getOrElse(0.0)

    /** The point `s` tiles along the route, held to its ends, and the route's direction there. */
    def at(s: Double): (Point, Point) =
      if (corners.size < 2) (corners.headOption.getOrElse((0.0, 0.0)), (1.0, 0.0))
      else {
        val clamped = s.max(0.0).min(length)
        val i       = (along.lastIndexWhere(_ <= clamped) max 0) min (corners.size - 2)
        val (a, b)  = (corners(i), corners(i + 1))
        val segment = distance(a, b)
        val dir     = if (segment <= 0) (1.0, 0.0) else ((b._1 - a._1) / segment, (b._2 - a._2) / segment)
        val into    = clamped - along(i)
        ((a._1 + dir._1 * into, a._2 + dir._2 * into), dir)
      }

    /** How far along the route lies the point of it nearest to `p`. */
    def progressOf(p: MapTilePosition): Double =
      if (corners.size < 2) 0.0
      else {
        val (x, y) = (p.x.toDouble, p.y.toDouble)
        corners.indices.dropRight(1).map { i =>
          val (a, b)  = (corners(i), corners(i + 1))
          val segment = distance(a, b)
          val t       =
            if (segment <= 0) 0.0
            else (((x - a._1) * (b._1 - a._1) + (y - a._2) * (b._2 - a._2)) / (segment * segment)).max(0.0).min(1.0)
          val on = (a._1 + (b._1 - a._1) * t, a._2 + (b._2 - a._2) * t)
          (distance(on, (x, y)), along(i) + segment * t)
        }.minBy(_._1)._2
      }
  }

  def distance(a: Point, b: Point) = math.hypot(a._1 - b._1, a._2 - b._2)

  /**
    * Every member's place, by its id and the reach of its weapon in tiles: how far behind the front it stands, and how
    * far across the route (positive to the left of the march).
    */
  def places(members: Seq[(Int, Double)]): Map[Int, Point] = {
    val longest = members.map(_._2).maxOption.getOrElse(0.0)
    members.groupBy((_, reach) => math.round((longest - reach) * 2) / 2.0).flatMap { (lag, band) =>
      band.sortBy(_._1).zipWithIndex.map { case ((id, _), i) =>
        val column = i % RowWidth
        val across = if (column % 2 == 0) column / 2 else -(column + 1) / 2
        id -> (lag + (i / RowWidth) * RowDepth, across.toDouble)
      }
    }
  }

  /** How deep the formation is: the lag of its hindmost place. */
  def depth(places: Map[Int, Point]) = places.values.map(_._1).maxOption.getOrElse(0.0)

  /** Where a member with this place stands while the front is `front` tiles along the route. */
  def slot(route: Route, front: Double, place: Point): Point = {
    val ((x, y), (dx, dy)) = route.at(front - place._1)
    (x + dy * place._2, y - dx * place._2)
  }

  /** The front moves on once enough of the army stands near its slots. */
  def keepsUp(distances: Seq[Double]) =
    distances.nonEmpty && distances.count(_ <= CohesionTiles) >= KeepUpShare * distances.size

  /** Where a front forms around an army: its median member halfway down the formation. */
  def formingFront(progress: Seq[Double], depth: Double) =
    if (progress.isEmpty) depth else progress.sorted.apply(progress.size / 2) + depth / 2
}
