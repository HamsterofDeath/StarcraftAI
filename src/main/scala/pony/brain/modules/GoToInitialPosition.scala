package pony
package brain
package modules

import scala.collection.mutable
import scala.reflect.ClassTag

class GoToInitialPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
  private val helper = new FormationAtFrontLineHelper(universe)

  private val ignore = mutable.HashSet.empty[Mobile]

  override protected def wrapBase(unit: Mobile) = {
    new SingleUnitBehaviour[Mobile](unit, meta) {
      override def describeShort: String = "Goto IP"

      override def toOrder(what: Objective) = {
        if (
          this.universe.time.minutes <= 5 || ignore(this.unit) || this.unit.isBeingCreated ||
          this.universe.pluginByType[RunTerranCampaign].isReservedDefender(this.unit)
        ) {
          Nil
        } else {
          helper.allInsideNonBlacklisted.iterator.nextOption().map { where =>
            ignore += this.unit
            helper.blacklisted(where)
            Orders.AttackMove(this.unit, where)
          }.toList
        }
      }
    }
  }
}
