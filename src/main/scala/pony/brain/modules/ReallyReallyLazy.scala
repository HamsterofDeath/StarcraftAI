package pony
package brain
package modules

import pony.units.Mobile

import scala.reflect.ClassTag

class ReallyReallyLazy(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

  override def priority = SecondPriority.None

  override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {

    override def isNoopTask = true

    override def describeShort = "Bored"

    override def toOrder(what: Objective) = {
      Orders.NoUpdate(this.unit).toList
    }
  }

}
