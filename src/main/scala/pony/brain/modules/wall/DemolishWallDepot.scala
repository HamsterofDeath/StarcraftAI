package pony
package brain
package modules
package wall

import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.combat.MobileRangeWeapon
import pony.units.SupplyDepot

/** One unit shells the chosen wall depot until the way out is open. */
private[pony] class DemolishWallDepot(
    attacker: MobileRangeWeapon,
    depot: SupplyDepot,
    owner: Employer[MobileRangeWeapon]
) extends UnitWithJob[MobileRangeWeapon](owner, attacker, Priority.Supply) with Interruptable[MobileRangeWeapon] {
  override def shortDebugString         = s"Open the wall at depot ${depot.nativeUnitId}"
  override def isFinished               = !depot.nativeUnit.exists || depot.isDead
  override def jobHasFailedWithoutDeath = !attacker.nativeUnit.exists || attacker.isDead
  override def everyNth                 = 23
  override def ordersForTick            = Orders.AttackUnit(attacker, depot).toSeq
  def targetId                          = depot.nativeUnitId
}
