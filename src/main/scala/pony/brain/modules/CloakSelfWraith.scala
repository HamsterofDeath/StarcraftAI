package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.reflect.ClassTag

class CloakSelfWraith(universe: Universe) extends DefaultBehaviour[Wraith](universe) {
  override protected def wrapBase(unit: Wraith) = new SingleUnitBehaviour[Wraith](unit, meta) {

    override def preconditionOk = upgrades.hasResearched(WraithCloak)

    override def toOrder(what: Objective) = {
      if (this.unit.isBeingAttacked && !this.unit.isCloaked) {
        List(Orders.TechOnSelf(this.unit, WraithCloak))
      } else {
        Nil
      }
    }

    override def describeShort: String = s"Cloak"
  }
}
