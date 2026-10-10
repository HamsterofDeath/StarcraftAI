package pony
package brain
package budget

import pony.brain.jobs.Employer
import pony.units.WrapsUnit

case class LockedResources[T <: WrapsUnit](
    reqs: ResourceRequests,
    proof: Option[ResourceApprovalSuccess],
    employer: Employer[T]
) {
  def equalTo(req: ResourceRequests, employer: Employer[T]) = req == reqs &&
    this.employer == employer

  def priority = reqs.priority

  def whatFor = reqs.whatFor
}
