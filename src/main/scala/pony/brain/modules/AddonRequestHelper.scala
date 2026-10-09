package pony
package brain
package modules

trait AddonRequestHelper extends AIModule[CanBuildAddons] {
  self =>

  private val helper = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper

  def requestAddon[T <: Addon](addonType: Class[? <: T],
                               handleDependencies: Boolean = false): Unit = {
    trace(s"Addon ${addonType.className} requested")
    val req = ResourceRequests.forUnit(race, addonType, Priority.Addon)
    val result = resources.request(req, self)
    requestAddonIfResourcesProvided(addonType, handleDependencies, result)
  }

  def requestAddonIfResourcesProvided[T <: Addon](addonType: Class[? <: T],
                                                  handleDependencies: Boolean,
                                                  result: ResourceApproval): Unit = {
    result.ifSuccess { suc =>
      trace(s"Addon ${addonType.className} requested using resources $suc")
      assert(resources.hasStillLocked(suc),
        s"This should never be called if $suc is no longer locked")
      val unitReq = UnitJobRequest.addonConstructor(self, addonType)
      trace(s"Financing possible for addon $addonType, requesting build")
      val result = unitManager.request(unitReq)
      if (handleDependencies && result.hasAnyMissingRequirements) {
        result.notExistingMissingRequiments.foreach { what =>
          helper.requestBuilding(what, handleDependencies)
        }
        trace(s"Requirement missing, unlocked resource for addon")
        resources.unlock_!(suc)
      } else if (!result.success) {
        trace(s"Did not get builder for addon, unlocking resources")
        resources.unlock_!(suc)
      } else {
        result.ifOne { one =>
          assignJob_!(new ConstructAddon(self, one, addonType, suc))
        }
      }
    }
  }
}
