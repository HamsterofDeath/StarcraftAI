package pony
package brain
package requests

import pony.brain.jobs.CanHireInfo
import pony.units.WrapsUnit

class PartialPreHiringResult[T <: WrapsUnit](override val canHire: CanHireInfo[T])
    extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = false
}
