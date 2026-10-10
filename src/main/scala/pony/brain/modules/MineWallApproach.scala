package pony
package brain
package modules

import pony.Upgrades.Terran._

/** A Vulture's trip out to mine the approach in front of the sealed wall: ferried out, mines laid, ferried back. */
private[pony] class MineWallApproach(
    vulture: Vulture,
    spots: Vector[MapTilePosition],
    home: MapTilePosition,
    owner: Employer[Vulture]
) extends UnitWithJob[Vulture](owner, vulture, Priority.Supply) with FerrySupport[Vulture] {
  private var left      = spots
  private val startedAt = currentTick
  private def outside   = ferryManager.wallSide(vulture.currentTile).contains(false)
  private def back      = ferryManager.wallSide(vulture.currentTile).contains(true)
  private def laying    = left.nonEmpty && vulture.spiderMineCount > 0

  override def shortDebugString = "Mine the wall approach"
  override def everyNth         = 11

  // out while mines are left to lay, home afterwards: the ferry logic carries the Vulture across the wall
  override protected def ferryDropTarget = if (laying) left.headOption else Some(home)

  override def isFinished = !laying && back

  override def jobHasFailedWithoutDeath = currentTick - startedAt > 24 * 240

  override def ordersForTick = {
    if (laying && outside) {
      val spot = left.head
      if (vulture.currentTile.distanceToIsLess(spot, 2) || !vulture.canCastNow(SpiderMines)) left = left.tail
      if (vulture.canCastNow(SpiderMines))
        left.headOption.orElse(Some(spot)).map(t => vulture.toOrder(SpiderMines, t)).toSeq
      else Nil
    } else Nil
  }
}
