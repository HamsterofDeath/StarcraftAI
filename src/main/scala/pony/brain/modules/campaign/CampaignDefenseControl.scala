package pony
package brain
package modules
package campaign

import pony.geometry.MapTilePosition

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
