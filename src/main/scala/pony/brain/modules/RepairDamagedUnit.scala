package pony
package brain
package modules

import scala.reflect.ClassTag

class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {

  private val helper = new NonConflictingTargets[Mechanic, SCV](
    universe = universe,
    rateTarget = m => PriorityChain(m.percentageHPOk),
    validTargetTest = _.isDamaged,
    subAccept = (m, t) => m.currentArea == t.currentArea,
    subRate = (m, t) => PriorityChain(-m.currentTile.distanceSquaredTo(t.currentTile)),
    own = true,
    allowReplacements = true
  )

  override def priority = SecondPriority.EvenMore

  override def onTick_!() = {
    super.onTick_!()
    helper.onTick_!()
  }

  override protected def wrapBase(unit: SCV) = new SingleUnitBehaviour[SCV](unit, meta) {

    override def onStealUnit() = {
      super.onStealUnit()
      helper.unlock_!(this.unit)
    }

    override def describeShort: String = "Repair unit"

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      helper.suggestTarget(this.unit).map { what =>
        Orders.RepairUnit(this.unit, what)
      }.toList
    }
  }
}
