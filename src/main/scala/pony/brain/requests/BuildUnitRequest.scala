package pony
package brain
package requests

import pony.brain.budget.{HasFunding, ResourceApproval}
import pony.terrain.ResourceArea
import pony.units.{Addon, Building, Mobile, Upgrader, WrapsUnit}

import pony.brain.modules.production.AlternativeBuildingSpot

case class BuildUnitRequest[T <: WrapsUnit](
    universe: Universe,
    typeOfRequestedUnit: Class[? <: T],
    amount: Int,
    funding: ResourceApproval,
    override val priority: Priority,
    customBuildingPosition: AlternativeBuildingSpot,
    belongsTo: Option[ResourceArea] = None
) extends UnitRequest[T] with HasFunding with HasUniverse {

  self =>

  if (funding.isSuccess) {
    universe.resources.informUsage(funding, this)
  }

  lazy val isAddon = classOf[Addon] >= typeOfRequestedUnit

  lazy val isUpgrader = classOf[Upgrader] >= typeOfRequestedUnit

  lazy val isBuilding = classOf[Building] >= typeOfRequestedUnit

  lazy val isMobile = classOf[Mobile] >= typeOfRequestedUnit

  if (customBuildingPosition.shouldUse) {
    assert(typeOfRequestedUnit.toUnitType.isBuilding)
  }

  override def proofForFunding = funding

  override def acceptable(unit: T): Boolean = false // refuse all that exist

  def customPosition = customBuildingPosition

  override def dispose(): Unit = {
    super.dispose()
    // if this is a unit build request, dispose resources if the job itself has not already done so
    if (unlocksResourcesOnDispose) {
      assert(stillLocksResources)
      trace(s"Unlock during dispose: $self")
      unlock_!()
    }
  }
}
