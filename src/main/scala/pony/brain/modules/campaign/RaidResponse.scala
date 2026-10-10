package pony
package brain
package modules
package campaign

/**
  * How far a raid on our bases reaches into the campaign. Home fighters always answer it; the campaign army turns
  * back only when home cannot hold and the raid is worth at least `RecallShare` of the army. Before, a single dragoon
  * at an outer field called a maxed army back across the map, launch after launch.
  */
private[pony] object RaidResponse {
  val RecallShare = 0.5

  /** Values are minerals plus gas of the raiders, of our fighters at home and of the campaign army out. */
  def recallsCampaign(threat: Int, home: Int, campaign: Int): Boolean =
    campaign == 0 || (threat > home && threat >= campaign * RecallShare)
}
