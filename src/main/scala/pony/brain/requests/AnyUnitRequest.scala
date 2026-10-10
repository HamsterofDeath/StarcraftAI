package pony
package brain
package requests

import pony.units.WrapsUnit

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[? <: T], amount: Int)
    extends UnitRequest[T] {
  override def priority: Priority = Priority.Default

}
