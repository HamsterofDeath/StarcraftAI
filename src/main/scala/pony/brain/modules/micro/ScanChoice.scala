package pony
package brain
package modules
package micro

import pony.geometry.MapTilePosition

/** Where a sweep pays off, kept free of the game. */
private[pony] object ScanChoice {
  enum Reason {
    case HiddenAttacker, UnseenDamage, HiddenDetector
  }

  /** `hunters`: our units near it that could shoot the revealed enemy. */
  final case class Candidate(at: MapTilePosition, reason: Reason, hunters: Int, about: String)

  /** A sweep reveals for about this long (262 frames). */
  val SweepFrames = 262

  /** A sweep covers about this many tiles around its centre. */
  val SweepRadius = 10

  /** Our shooters this close to a hidden enemy can kill it once it is revealed. */
  val HuntRadius = 9

  /** A visible enemy this close to a damaged unit of ours may have dealt the damage. */
  val VisibleCulpritRadius = 8

  /**
    * The best place to sweep, if any: only where someone can shoot what the sweep reveals, never inside a sweep still
    * running; a hidden attacker before unexplained damage, unexplained damage before a hidden detector, more hunters
    * first.
    */
  def choose(candidates: Seq[Candidate], sweeping: Seq[MapTilePosition]): Option[Candidate] =
    candidates
      .filter(c => c.hunters > 0 && !sweeping.exists(_.distanceToIsLess(c.at, SweepRadius)))
      .sortBy(c => (c.reason.ordinal, -c.hunters))
      .headOption
}
