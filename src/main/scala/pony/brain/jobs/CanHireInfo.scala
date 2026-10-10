package pony
package brain
package jobs

import pony.brain.requests.UnitJobRequest
import pony.units.WrapsUnit

case class CanHireInfo[T <: WrapsUnit](request: Option[UnitJobRequest[T]], details: Set[T])

object CanHireInfo {
  def empty[T <: WrapsUnit] = CanHireInfo[T](None, Set.empty)
}
