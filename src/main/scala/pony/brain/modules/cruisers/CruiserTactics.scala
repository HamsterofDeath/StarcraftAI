package pony
package brain
package modules
package cruisers

import pony.geometry.MapTilePosition

/** The fleet decisions of CruiserRaids, kept free of the game. */
private[pony] object CruiserTactics {

  /** Below this share of its hit points a cruiser flies home for repair... */
  val RetreatBelow = 0.4

  /** ...and from this share on it is fit to raid again. */
  val FitFrom = 0.9

  /** A raid starts with this many fit cruisers... */
  val RaidSize = 3

  /** ...and ends when fewer than two stay on it or their mean health falls below this share. */
  val WornBelow = 0.55

  val BerthDistance = 6

  val RetreatPriority = SecondPriority(0.95)

  /** Stepping apart under storm threat outranks the ranged micro but not a hurt cruiser's retreat. */
  val SpacingPriority = SecondPriority(0.94)

  /** Tiles between spread cruisers: a storm covers three, so two tiles apart it catches one or two at most. */
  val SpreadTiles = 2

  /** A High Templar this close to a raider spreads the raid; storm range is nine tiles. */
  val TemplarSight = 12

  /** A bank this long without 50 minerals or 50 gas can no longer pay for repairs. */
  val StarvedFrames = 24 * 120

  def needsRepair(health: Double, repairing: Boolean) = health < (if (repairing) FitFrom else RetreatBelow)

  /**
    * At least RaidSize fit cruisers and two thirds of the fleet: the hurt are mended before the fleet sets out. A big
    * fleet attacks as one group, so it waits for four fifths.
    */
  def startsRaid(fit: Int, fleet: Int) =
    fit >= RaidSize && (if (fleet >= BigFleet) fit * 5 >= fleet * 4 else fit * 3 >= fleet * 2)

  /** From this many cruisers on the fleet attacks as one group. */
  val BigFleet = 9

  /** Minerals and gas of one cruiser, the measure of a raid's strength (times its health). */
  val CruiserValue = 700

  /** Tiles around the raid's centre in which enemy anti-air counts against it. */
  val RaidSight = 12

  /**
    * A raid hits and runs: it leaves once the anti-air near it reaches this share of its own value; a big group stays
    * longer. Cruisers are dear for what they fight: by the square law three of them (2,100) only break even with about
    * six Dragoons (1,050), and at 0.8 raids fought on until worn (game 1 on e643b1b ended seven of eight raids worn).
    */
  val RunRatio      = 0.45
  val GroupRunRatio = 0.8

  def outnumbered(antiAir: Int, strength: Double, bigGroup: Boolean) =
    antiAir > strength * (if (bigGroup) GroupRunRatio else RunRatio)

  /**
    * The anti-air a raid met at a target counts against the next raid there for this long: in game 1 on 1f86417 raids
    * of three went back to the same base seven times, meeting up to 1,500 of anti-air, until one cruiser was left.
    */
  val DefenceMemory = 24 * 60 * 4

  def remembered(seen: Option[(Int, Int)], now: Int): Int =
    seen.collect { case (antiAir, at) if now - at < DefenceMemory => antiAir }.getOrElse(0)

  /**
    * A raid sets out against at most this share of its strength in remembered anti-air; else the fleet waits and grows.
    * Below the run ratios, since a raid gets hurt before it runs.
    */
  val StartRatio      = 0.3
  val GroupStartRatio = 0.6

  def strongEnough(fit: Int, defence: Int, bigGroup: Boolean) =
    defence <= fit * CruiserValue * (if (bigGroup) GroupStartRatio else StartRatio)

  /** Gathered once every raider is this close to the centre, or after GatherFrames at the latest. */
  val GatherRadius = 6
  val GatherFrames = 24 * 30

  def gathered(raiders: Seq[MapTilePosition], centre: MapTilePosition) =
    raiders.forall(r => !r.distanceToIsMore(centre, GatherRadius))

  /** For this long after gathering the leaders wait for the group; then everyone flies straight at the target. */
  val CohesionFrames = 24 * 90

  /** A raid whose centre stays within LingerTiles of its target for LingerFrames, destroying nothing, gives it up. */
  val LingerTiles  = 6
  val LingerFrames = 24 * 90

  /** A raid whose centre gains less than ProgressTiles on its target in StallFrames gives the target up. */
  val ProgressTiles = 3
  val StallFrames   = 24 * 120

  /** A raider more than StrayRadius tiles from the centre, on the target's side of it, waits for the others. */
  val StrayRadius = 8

  def waitsForGroup(me: MapTilePosition, centre: MapTilePosition, target: MapTilePosition) =
    me.distanceToIsMore(centre, StrayRadius) && me.distanceSquaredTo(target) < centre.distanceSquaredTo(target)

  def endsRaid(health: Seq[Double]) = health.size < 2 || health.sum / health.size < WornBelow

  /** A crew SCV whose cruiser gains no hit points for this long gives it up for CrewGiveUpFrames. */
  val CrewStallFrames  = 24 * 6
  val CrewGiveUpFrames = 24 * 30

  def crewStalled(now: Int, lastGain: Int) = now - lastGain > CrewStallFrames

  /** Two to ten SCVs, one more for every two cruisers. */
  def crewSize(cruisers: Int) = if (cruisers == 0) 0 else (2 + cruisers / 2) min 10

  /**
    * A known enemy base that is no start location (an expansion is defended least), else an unvisited site where the
    * enemy likely expanded, else a base, else an enemy building, else an enemy start not yet found empty. Within each,
    * the one with the least enemy army seen near it lately (strike where their army is not), then the nearest.
    */
  def choose(
      enemyBases: Seq[MapTilePosition],
      likelyExpansions: Seq[MapTilePosition],
      enemyBuildings: Seq[MapTilePosition],
      enemyStarts: Seq[MapTilePosition],
      home: MapTilePosition,
      armyNear: MapTilePosition => Double = _ => 0.0
  ): Option[MapTilePosition] = {
    def nearest(tiles: Seq[MapTilePosition]) = tiles.minByOpt(t => (armyNear(t), t.distanceSquaredTo(home)))
    val expansions                           = enemyBases.filterNot(b => enemyStarts.exists(_.distanceToIsLess(b, 8)))
    nearest(expansions).orElse(nearest(likelyExpansions)).orElse(nearest(enemyBases))
      .orElse(nearest(enemyBuildings)).orElse(nearest(enemyStarts))
  }
}
