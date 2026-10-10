package pony
package brain
package modules
package micro

import pony.units.Ghost

import pony.tech.Upgrades.Terran._

import scala.reflect.ClassTag

class CloakSelfGhost(universe: Universe) extends DefaultBehaviour[Ghost](universe) {
  override protected def wrapBase(unit: Ghost) = new SingleUnitBehaviour[Ghost](unit, meta) {

    override def preconditionOk = upgrades.hasResearched(GhostCloak)

    override def toOrder(what: Objective) = {
      if (this.unit.isBeingAttacked && !this.unit.isCloaked) {
        List(Orders.TechOnSelf(this.unit, GhostCloak))
      } else {
        Nil
      }
    }

    override def describeShort: String = s"Cloak"
  }
}
