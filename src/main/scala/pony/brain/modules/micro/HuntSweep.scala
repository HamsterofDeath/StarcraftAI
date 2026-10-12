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

  /** Tiles from a hunt point within which an army unit counts as having looked there. */
  val VisitTiles = 10

  /** Frames after which a hunt point counts as visited however far the army got: it may be out of reach. */
  val VisitFrames = 24 * 180

  /**
    * A hunt point is done once half the army stands near it, or after VisitFrames. The campaign army never disbands by
    * itself, so waiting for it to end held a 74-unit army on one empty point for 32 minutes (game 4 vs Zerg, a82be0c).
    */
  def visited(near: Int, army: Int, framesSinceLaunch: Int): Boolean =
    framesSinceLaunch > VisitFrames || army > 0 && near * 2 >= army
}
