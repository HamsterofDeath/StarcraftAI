package pony
package brain
package modules

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T]) = {
    val req = ResourceRequests.forUnit(buildingType)
    val result = resources.request(req, buildingEmployer)
    result match {
      case suc: ResourceApprovalSuccess =>
        val unitReq = UnitJobRequests.newOfType(universe, buildingEmployer, buildingType, suc)
        unitManager.request(unitReq)
    }
  }
}

class ProvideFactories(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  override def onTick(): Unit = {
    evaluateCapacities.foreach { cap =>
      val existingByType = unitManager.unitsByType(cap.typeOfFactory).size +
                           unitManager.plannedToBuildByType(cap.typeOfFactory)
      if (existingByType < cap.maximumSustainable) {
        requestBuilding(cap.typeOfFactory)
      }
    }
  }

  private def evaluateCapacities = {
    // TODO actually calculate this & adjust number of factories to money income
    IdealProducerCount(classOf[Barracks], 3) ::
    IdealProducerCount(classOf[Factory], 3) ::
    IdealProducerCount(classOf[Starport], 3) ::
    Nil
  }
  case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory], maximumSustainable: Int)
}

class ProvideArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  override def onTick(): Unit = {

  }
}