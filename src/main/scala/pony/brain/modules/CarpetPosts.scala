package pony
package brain
package modules

import scala.collection.mutable

/** Pure farthest-point ordering so the carpet posts spread evenly over the map. */
private[pony] object CarpetPosts {
  def order(candidates: Vector[MapTilePosition], count: Int): Vector[MapTilePosition] = {
    val distinct = candidates.distinct
    if (distinct.isEmpty || count <= 0) Vector.empty
    else {
      val first     = distinct.minBy(t => (t.y, t.x))
      val chosen    = mutable.ArrayBuffer(first)
      val remaining = mutable.Set.empty[MapTilePosition] ++ distinct
      remaining.remove(first)
      while (chosen.size < count && remaining.nonEmpty) {
        val next = remaining.maxBy { t =>
          val distances = chosen.iterator.map(c => t.distanceSquaredTo(c)).toVector
          (distances.min, distances.sum, -t.y, -t.x)
        }
        chosen += next
        remaining.remove(next)
      }
      chosen.toVector
    }
  }
}
