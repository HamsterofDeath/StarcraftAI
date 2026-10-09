package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.reflect.ClassTag

class StimSelf(universe: Universe) extends DefaultBehaviour[CanUseStimpack](universe) {
  override protected def wrapBase(unit: CanUseStimpack) = new SingleUnitBehaviour[CanUseStimpack](unit, meta) {

    override def preconditionOk = upgrades.hasResearched(InfantryCooldown)

    override def toOrder(what: Objective) = {
      if (this.unit.isAttacking && !this.unit.isStimmed) {
        List(Orders.TechOnSelf(this.unit, InfantryCooldown))
      } else {
        Nil
      }
    }

    override def describeShort: String = s"Stimpack"
  }
}
