package pony
package brain
package modules
package economy

import pony.units.WorkerUnit

import scala.reflect.ClassTag

class DeliverResources(universe: Universe) extends DefaultBehaviour[WorkerUnit](universe) {

  override def priority = SecondPriority.BetterThanNothing

  override protected def wrapBase(unit: WorkerUnit) = new SingleUnitBehaviour(unit, meta) {
    override def describeShort = "Return resources"

    override protected def toOrder(what: Objective) = {
      if (this.unit.isCarryingGas || this.unit.isCarryingMinerals) {
        Orders.ReturnResourcesToAnyBase(this.unit).toList
      } else {
        Nil
      }
    }
  }
}
