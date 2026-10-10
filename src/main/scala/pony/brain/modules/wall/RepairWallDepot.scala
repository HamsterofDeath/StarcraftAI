package pony
package brain
package modules
package wall

import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.units.{SCV, SupplyDepot}

/** An SCV patches a wall depot while it is under attack and returns to mining afterwards. */
private[pony] class RepairWallDepot(worker: SCV, depot: SupplyDepot, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString = s"Repair wall depot ${depot.nativeUnitId}"
  private def state             = WallRepairState(
    worker.nativeUnit.exists && !worker.isDead,
    depot.nativeUnit.exists,
    depot.nativeUnit.getHitPoints < depot.nativeUnit.getType.maxHitPoints,
    depot.isFloating
  )
  override def isFinished               = state == WallRepairState.Finished
  override def jobHasFailedWithoutDeath = state == WallRepairState.Failed
  override def everyNth                 = 23
  override def ordersForTick            = Orders.RepairBuilding(worker, depot).toSeq
  def targetId                          = depot.nativeUnitId
}
