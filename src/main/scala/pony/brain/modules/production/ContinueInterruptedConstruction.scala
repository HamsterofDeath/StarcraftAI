package pony
package brain
package modules
package production

import pony.brain.modules.micro.NonConflictingTargets
import pony.units.{Addon, Building, SCV}

import scala.reflect.ClassTag

class ContinueInterruptedConstruction(universe: Universe)
    extends DefaultBehaviour[SCV](universe) {

  private val area = oncePerTick {
    mapLayers.rawWalkableMap.guaranteeImmutability
  }
  private val helper = new NonConflictingTargets[Building, SCV](
    universe = universe,
    rateTarget = b => PriorityChain(-b.remainingBuildTime),
    validTargetTest = e => e.isIncompleteAbandoned && !e.isInstanceOf[Addon],
    subRate = (w, b) => PriorityChain(-w.currentTile.distanceSquaredTo(b.tilePosition)),
    own = true,
    allowReplacements = true,
    subAccept = (w, b) => true
  )

  override def priority = SecondPriority.Max

  override def onTick_!() = {
    super.onTick_!()
    helper.onTick_!()
  }

  override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {

    private var target = Option.empty[Building]

    override def canInterrupt = {
      super.canInterrupt && !this.unit.isInConstructionProcess &&
      (target.isEmpty || !target.exists(_.incomplete))
    }

    override def describeShort: String = "Finish construction"

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      def eval = helper.suggestTarget(this.unit)
      target.filter(_.incomplete).orElse(eval).map { building =>
        target = Some(building)
        val sameArea = area.areInSameWalkableArea(this.unit.currentTile, building.centerTile)
        if (sameArea) {
          Orders.ContinueConstruction(this.unit, building)
        } else {
          Orders.MoveToTile(this.unit, building.centerTile)
        }
      }.toList
    }
  }
}
