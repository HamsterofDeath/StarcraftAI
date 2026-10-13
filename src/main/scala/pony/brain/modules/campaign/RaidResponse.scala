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
}
