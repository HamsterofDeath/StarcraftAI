package pony
package brain
package modules
package economy

import pony.brain.modules.micro.NonConflictingTargets
import pony.units.{Mechanic, SCV}

import scala.reflect.ClassTag

class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {

  private val helper = new NonConflictingTargets[Mechanic, SCV](
    universe = universe,
    rateTarget = m => PriorityChain(m.percentageHPOk),
    validTargetTest = _.isDamaged,
    // a repairer walks only to nearby units it can reach; the damaged come home to be mended
    subAccept = (m, t) =>
      m.currentArea == t.currentArea && m.currentTile.distanceToIsLess(t.currentTile, 20) &&
        !ferryManager.sealedApart(m.currentTile, t.currentTile),
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

    private var tracedTarget = Option.empty[Int]

    override def describeShort: String = "Repair unit"

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      val target = helper.suggestTarget(this.unit)
      val id     = target.map(_.nativeUnitId)
      if (id != tracedTarget) {
        id.foreach(t => NativeMatchEvidence.trace("repair", s"scv=${this.unit.nativeUnitId} target=$t"))
        tracedTarget = id
      }
      target.map(Orders.RepairUnit(this.unit, _)).toList
    }
  }
}
