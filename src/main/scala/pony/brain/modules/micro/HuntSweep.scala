package pony
package brain
package modules
package micro

import pony.geometry.MapTilePosition

/** The endgame hunt sweeps every resource area, farthest from home first, then starts over. */
private[pony] object HuntSweep {
  def order(points: Seq[MapTilePosition], home: Option[MapTilePosition]): Vector[MapTilePosition] =
    points.toVector.sortBy(p => home.map(h => -p.distanceSquaredTo(h)).getOrElse(0))
  def next(size: Int, index: Int): Int = if (size <= 0) index else (index + 1) % size
}
