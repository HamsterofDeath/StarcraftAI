package pony
package brain
package modules

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

/** A raid invalidates in-flight offensive plans and survives a busy planner until admitted. */
private[pony] class CampaignDefenseControl {
  var pressure                        = false
  var recallsCampaign                 = true
  var generation                      = 0
  var target                          = Option.empty[MapTilePosition]
  private var pending                 = Option.empty[MapTilePosition]
  def setPressure(now: Boolean): Unit = {
    if (now != pressure) generation += 1
    pressure = now
    if (!now) { target = None; pending = None }
  }
  def queue(where: MapTilePosition): Unit                   = { target = Some(where); pending = Some(where) }
  def takeReady(planning: Boolean): Option[MapTilePosition] = {
    if (planning) None else { val result = pending; pending = None; result }
  }
  def acceptsCampaign(version: Int) = !pressure && generation == version
}
