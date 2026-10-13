package pony
package brain
package modules
package campaign

/**
  * How far a raid on our bases reaches into the campaign. Home fighters always answer it; the campaign army turns
  * back only when home cannot hold and the raid is worth at least `RecallShare` of the army. Before, a single dragoon
  * at an outer field called a maxed army back across the map, launch after launch. A smaller raid home cannot hold
  * draws a detachment worth `DetachShare` times the raid from the campaign instead: in game 1 vs Terran on d96d96e a
  * raid worth 500 met no one at home while the army of 2625 marched on, and the base died worker by worker.
  */
private[pony] object RaidResponse {
  val RecallShare = 0.5
  val DetachShare = 2

  /** Values are minerals plus gas of the raiders, of our fighters at home and of the campaign army out. */
  def recallsCampaign(threat: Int, home: Int, campaign: Int): Boolean =
    campaign == 0 || (threat > home && threat >= campaign * RecallShare)

  /** The value the campaign sends home when the whole army does not turn back; 0 when home holds alone. */
  def detachment(threat: Int, home: Int, campaign: Int): Int =
    if (recallsCampaign(threat, home, campaign) || threat <= home) 0
    else (threat * DetachShare - home).min(campaign)

  /**
    * Frames after a raid beyond our closed wall began during which home fighters stay behind it whatever they see. In
    * game 1 vs Terran on ICCup Lost_Temple at 0cc8d1c a lone Medic in sight counted 75 against 600 at home; eleven
    * Marines filed out through the wall into twelve Marines and three Medics coming round the corner and died without a
    * kill. Four seconds later the enemy column stood in sight of our depots.
    */
  val LookFrames = 24 * 4

  /** Frames an enemy fighter out of sight still counts toward the force near a raid, where it was last seen. */
  val SeenFrames = 24 * 10

  /**
    * Whether home fighters stay behind our closed wall rather than go out to a raid beyond it, given the worth of the
    * enemy force seen near the raid lately (None when the wall does not keep the raid out) and the frames since the raid
    * began. They stay while they look, then while that force outweighs them; once staying, until they are worth half
    * as much again, so that enemies stepping in and out of sight do not send them out and call them back again and again.
    */
  def holdsBehindWall(force: Option[Int], home: Int, holding: Boolean, sinceRaid: Int): Boolean =
    force.exists(f => sinceRaid < LookFrames || (if (holding) f * 3 > home * 2 else f > home))
}
