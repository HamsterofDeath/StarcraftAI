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
  val VisitFrames = 24 * 120

  /** A squad of this many at a hunt point counts as having looked there, however large the army behind it is. */
  val VisitSquad = 12

  /**
    * A hunt point is done once half the army stands near it, or after VisitFrames. The campaign army never disbands by
    * itself, so waiting for it to end held a 74-unit army on one empty point for 32 minutes (game 4 vs Zerg, a82be0c).
    */
  def visited(near: Int, army: Int, framesSinceLaunch: Int): Boolean =
    framesSinceLaunch > VisitFrames || near >= VisitSquad || army > 0 && near * 2 >= army

  /**
    * Comsats with this much energy sweep hunt points nobody looks at (UseComsat), and keep one sweep for hidden
    * enemies. In game 4 vs Zerg on af1b88d the army of 46 spent the full 180 seconds on each of the empty hunt points
    * (5,85) and (5,120) while five comsats had not swept since frame 36025.
    */
  val ScanMana = 100

  /** A swept hunt point shows what stands there this many frames after the sweep is cast... */
  val ScanShownFrames = 48

  /** ...and counts as looked at for this long. */
  val ScanFreshFrames = 24 * 90

  /** Whether a sweep cast at `scannedAt` showed the hunt point lately. */
  def clearedByScan(scannedAt: Option[Int], now: Int) =
    scannedAt.exists(at => now - at >= ScanShownFrames && now - at < ScanFreshFrames)

  /** The first hunt point from `index` on that no sweep showed lately, or `index` when sweeps showed them all. */
  def nextUnscanned(size: Int, index: Int, cleared: Int => Boolean): Int =
    if (size <= 0) index
    else (0 until size).iterator.map(i => (index + i) % size).find(i => !cleared(i)).getOrElse(index % size)
}
