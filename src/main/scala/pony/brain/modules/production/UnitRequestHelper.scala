package pony
package brain
package modules
package production

import pony.brain.budget.{ResourceApprovalSuccess, ResourceRequests}
import pony.brain.jobs.Employer
import pony.brain.requests.UnitJobRequest
import pony.units.{Addon, CanBuildAddons, Mobile, UnitFactory, WorkerUnit}

trait UnitRequestHelper extends AIModule[UnitFactory] {
  private val mobileEmployer = new Employer[Mobile](universe)

  private val buildingHelper = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val addonHelper    = new HelperAIModule[CanBuildAddons](universe) with AddonRequestHelper

  protected def mobileCost[T <: Mobile](mobileType: Class[? <: T], priority: Priority) =
    ResourceRequests.forUnit(race, mobileType, priority)
  protected def mobileRequest[T <: Mobile](
      mobileType: Class[? <: T],
      funding: ResourceApprovalSuccess,
      priority: Priority
  ) =
    UnitJobRequest.newOfType(universe, mobileEmployer, mobileType, funding, priority = priority)

  def requestUnit[T <: Mobile](
      mobileType: Class[? <: T],
      takeCareOfDependencies: Boolean,
      priority: Priority = Priority.Default
  ) = {
    val req    = mobileCost(mobileType, priority)
    var ok     = false
    val result = resources.request(req, mobileEmployer)
    result.ifSuccess { suc =>
      val unitReq = mobileRequest(mobileType, suc, priority)
      trace(s"Financing possible for mobile unit $mobileType, requesting training")
      val result = unitManager.request(unitReq)
      if (!MobileRequestAdmission.accept(result)(resources.unlock_!(suc))) {
        // do not forget to unlock the resources again
        trace(s"Requirement missing for $mobileType, unlocking resource")
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { requirement =>
            trace(s"Checking dependency: $requirement")
            if (!unitManager.existsOrPlanned(requirement)) {
              def isAddon = classOf[Addon] >= requirement
              trace(s"Planning to build $requirement because it is required for $mobileType")
              if (isAddon) {
                addonHelper.requestAddon(requirement.asInstanceOf[Class[? <: Addon]])
              } else {
                buildingHelper.requestBuilding(requirement, takeCareOfDependencies = false)
              }
            }
          }
        }
      } else {
        ok = true
      }
    }
    ok
  }
}
