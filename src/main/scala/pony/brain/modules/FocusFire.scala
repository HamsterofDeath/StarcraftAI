package pony
package brain
package modules

import scala.reflect.ClassTag

class FocusFire(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

  private val helper = new FocusFireOrganizer(universe)

  override def onTick_!() = {
    super.onTick_!()
    helper.onTick_!()
  }

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    helper.renderDebug_!(renderer)
  }

  override protected def wrapBase(unit: MobileRangeWeapon) = new SingleUnitBehaviour(unit, meta) {
    override def describeShort = "Focus fire"

    override def toOrder(what: Objective) = {
      helper.suggestTarget(this.unit).map { target =>
        Orders.AttackUnit(this.unit, target)
      }.toList
    }
  }
}
