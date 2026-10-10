package pony
package brain
package modules
package micro

import pony.combat.ArmedMobile

import scala.reflect.ClassTag

class HelpNearUnits(universe: Universe) extends DefaultBehaviour[ArmedMobile](universe) {

  override def priority = SecondPriority.Less

  override protected def wrapBase(unit: ArmedMobile) = {
    new SingleUnitBehaviour[ArmedMobile](unit, meta) {
      override def describeShort = "(+)"

      override protected def toOrder(what: Objective) = {
        val closest = {
          this.unit.surroundings.closeOwnUnits
            .iterator
            .filter(_.isInFight)
            .filter { e =>
              mapLayers.rawWalkableMap.connectedByLine(e.currentTile, this.unit.currentTile)
            }
            .minByOpt(_.currentTile.distanceSquaredTo(this.unit.currentTile))
        }
        closest.map { helpThisOne =>
          Orders.AttackMove(this.unit, helpThisOne.currentTile)
        }.toList
      }
    }
  }
}
