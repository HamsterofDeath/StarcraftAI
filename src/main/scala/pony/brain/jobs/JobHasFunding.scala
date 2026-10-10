package pony
package brain
package jobs

import pony.brain.budget.HasFunding
import pony.units.WrapsUnit

trait JobHasFunding[T <: WrapsUnit] extends UnitWithJob[T] with HasUniverse with HasFunding {
  self =>

  assert(proofForFunding.isSuccess, s"Problem, check $this")

  resources.informUsage(proofForFunding, this)

  def canReleaseResources = ageSinceLastReset == 0 && stillLocksResources

  override def notifyResourcesDisapproved_!(): Unit = {
    super.notifyResourcesDisapproved_!()
    fail_!()
  }

  listen_!(failed => {
    trace(s"Unlock because $self failed")
    unlock_!()
  })
}
