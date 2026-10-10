package pony
package brain
package modules
package production

import pony.brain.budget.ResourceRequests
import pony.brain.jobs.Employer
import pony.brain.requests.{BuildUnitRequest, UnitJobRequest}
import pony.terrain.ResourceArea
import pony.units.{Building, UpgradeLimitLifter, WorkerUnit}

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer                                                      = new Employer[Building](universe)
  protected def onBuildingRequested(request: BuildUnitRequest[? <: Building]): Unit = {}

  def requestBuilding[T <: Building](
      buildingType: Class[? <: T],
      takeCareOfDependencies: Boolean = false,
      saveMoneyIfPoor: Boolean = false,
      customBuildingPosition: AlternativeBuildingSpot =
        AlternativeBuildingSpot
          .useDefault,
      belongsTo: Option[ResourceArea] = None,
      priority: Priority = Priority.Default
  ): Unit = {

    val isUpgrader = classOf[UpgradeLimitLifter].isAssignableFrom(buildingType)
    val satisfied  = {
      isUpgrader && unitManager.countExistingAndPlanned(buildingType) >= 2
    }
    if (satisfied) {
      warn(s"Too many buildings of type $buildingType requested!")
    } else {
      val req    = ResourceRequests.forUnit(race, buildingType, priority)
      val result = resources.request(req, buildingEmployer)
      result.ifSuccess { suc =>
        val unitReq = UnitJobRequest.newOfType(
          universe,
          buildingEmployer,
          buildingType,
          suc,
          customBuildingPosition = customBuildingPosition,
          belongsTo = belongsTo,
          priority = priority
        )
        onBuildingRequested(unitReq.request.asInstanceOf[BuildUnitRequest[T]])
        trace(s"Financing possible for building $buildingType, requesting build")
        val result = unitManager.request(unitReq)
        if (result.hasAnyMissingRequirements) {
          resources.unlock_!(suc)
        }
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { what =>
            if (!unitManager.requestedToBuild(what)) {
              requestBuilding(
                what,
                takeCareOfDependencies,
                saveMoneyIfPoor,
                AlternativeBuildingSpot.useDefault,
                belongsTo,
                priority = priority
              )
            }
          }
        }
      }
      if (result.failed && saveMoneyIfPoor) {
        resources.forceLock_!(req, buildingEmployer)
      }
    }
  }
}
