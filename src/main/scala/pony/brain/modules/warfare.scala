package pony
package brain
package modules

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T],
                                     forceBuildingPosition: Option[MapTilePosition] = None) = {
    val req = ResourceRequests.forUnit(universe.race, buildingType)
    val result = resources.request(req, buildingEmployer)
    result match {
      case suc: ResourceApprovalSuccess =>
        val unitReq = UnitJobRequests.newOfType(universe, buildingEmployer, buildingType, suc,
          forceBuildingPosition = forceBuildingPosition)
        info(s"Financing possible for $buildingType, requesting build")
        unitManager.request(unitReq)
      case _ =>
    }
  }
}

trait UnitRequestHelper extends AIModule[UnitFactory] {
  private val mobileEmployer = new Employer[Mobile](universe)

  def requestUnit[T <: Mobile](mobileType: Class[_ <: T]) = {
    val req = ResourceRequests.forUnit(universe.race, mobileType)
    val result = resources.request(req, mobileEmployer)
    result match {
      case suc: ResourceApprovalSuccess =>
        val unitReq = UnitJobRequests.newOfType(universe, mobileEmployer, mobileType, suc)
        info(s"Financing possible for $mobileType, requesting training")
        unitManager.request(unitReq)
      case _ =>
    }
  }
}

class ProvideFactories(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  override def onTick(): Unit = {
    evaluateCapacities.foreach { cap =>
      val existingByType = unitManager.unitsByType(cap.typeOfFactory).size +
                           unitManager.plannedToBuildByClass(cap.typeOfFactory).size
      if (existingByType < cap.maximumSustainable) {
        requestBuilding(cap.typeOfFactory)
      }
    }
  }

  private def evaluateCapacities = {
    // TODO actually calculate this & adjust number of factories to money income
    IdealProducerCount(classOf[Barracks], 6) ::
    /*
        IdealProducerCount(classOf[Factory], 3) ::
        IdealProducerCount(classOf[Starport], 3) ::
    */
    Nil
  }
  case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory], maximumSustainable: Int)
}

class ProvideArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  override def onTick(): Unit = {
    // for now, just spam marines
    requestUnit(classOf[Marine])
  }
}