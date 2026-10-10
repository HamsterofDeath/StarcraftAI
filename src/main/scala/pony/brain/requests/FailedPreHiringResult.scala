package pony
package brain
package requests

import pony.brain.jobs.CanHireInfo
import pony.units.WrapsUnit

class FailedPreHiringResult[T <: WrapsUnit] extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty
}
