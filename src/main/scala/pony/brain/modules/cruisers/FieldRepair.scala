package pony
package brain
package modules
package cruisers

import pony.geometry.MapTilePosition

/** When a raid wants its field crew, kept free of the game. */
private[pony] object FieldRepair {

  /** SCVs of a field crew. */
  val CrewSize = 3

  /** A raid whose centre moved less than this many tiles over StationaryFrames holds still. */
  val StationaryTiles  = 3
  val StationaryFrames = 24 * 10

  /** Cruisers below this share of their hit points count as hurt; this many hurt call the crew. */
  val HurtBelow  = 0.7
  val HurtNeeded = 2

  /** Enemy ground fighters this close to the spot keep the crew away. */
  val SafeTiles = 8

  val ArrivedTiles = 4
  val ReachTiles   = 8
  val MaxFrames    = 24 * 300

  /** Whether the raid calls its crew: holding still, enough cruisers hurt, no enemy ground fighters at the spot. */
  def wanted(stationary: Boolean, hurt: Int, enemyGroundNear: Boolean) =
    stationary && hurt >= HurtNeeded && !enemyGroundNear

  /** Whether the centre stayed within StationaryTiles over the trail (oldest first) covering StationaryFrames. */
  def stationary(trail: Seq[(Int, MapTilePosition)], now: Int): Boolean =
    trail.headOption.exists(_._1 <= now - StationaryFrames) && {
      val recent = trail.filter(_._1 >= now - StationaryFrames).map(_._2)
      recent.forall(a => recent.forall(b => !a.distanceToIsMore(b, StationaryTiles)))
    }
}
