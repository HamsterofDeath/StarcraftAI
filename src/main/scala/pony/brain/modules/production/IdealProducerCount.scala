package pony
package brain
package modules
package production

import pony.units.UnitFactory

case class IdealProducerCount[T <: UnitFactory](
    typeOfFactory: Class[? <: UnitFactory],
    maximumSustainable: Int
)(
    active: => Boolean,
    highPriority: => Boolean = false
) {

  def withNewMaximum(sum: Int) = {
    IdealProducerCount(typeOfFactory, sum)(active, highPriority)
  }

  def format = {
    s"${if (active) "A" else "I"}, $maximumSustainable"
  }

  def highPriorityNow = highPriority

  def isActive = active
}
