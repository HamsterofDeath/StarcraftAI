package pony
package brain
package modules

/** Temporary custody steals only local mineral/idle SCVs, and returns them through the normal market. */
private[pony] class RepairDefensiveBunker(worker: SCV, bunker: Bunker, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString = s"Repair bunker ${bunker.nativeUnitId}"
  private def state             = BunkerRepairState(
    worker.nativeUnit.exists && !worker.isDead,
    bunker.nativeUnit.exists,
    bunker.nativeUnit.getHitPoints < bunker.nativeUnit.getType.maxHitPoints,
    bunker.isFloating
  )
  override def isFinished               = state == BunkerRepairState.Finished
  override def jobHasFailedWithoutDeath = state == BunkerRepairState.Failed
  override def everyNth                 = 23
  override def ordersForTick            = Orders.RepairBuilding(worker, bunker).toSeq
  def targetId                          = bunker.nativeUnitId
}
