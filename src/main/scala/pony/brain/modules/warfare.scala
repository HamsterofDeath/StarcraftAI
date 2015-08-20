package pony
package brain
package modules

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T],
                                     takeCareOfDependencies: Boolean,
                                     forceBuildingPosition: Option[MapTilePosition] = None): Unit = {
    val req = ResourceRequests.forUnit(universe.race, buildingType)
    val result = resources.request(req, buildingEmployer)
    result match {
      case suc: ResourceApprovalSuccess =>
        val unitReq = UnitJobRequests.newOfType(universe, buildingEmployer, buildingType, suc,
          forceBuildingPosition = forceBuildingPosition)
        info(s"Financing possible for $buildingType, requesting build")
        val result = unitManager.request(unitReq)
        if (takeCareOfDependencies) {
          if (result.hasMissingRequirements) {
            resources.unlock_!(suc)
          }
          result.missingRequirements.foreach { what =>
            if (!unitManager.plannedToBuild(what)) {
              requestBuilding(what, takeCareOfDependencies = false)
            }
          }
        }
      case _ =>
    }
  }
}

trait UnitRequestHelper extends AIModule[UnitFactory] {
  private val mobileEmployer = new Employer[Mobile](universe)

  private val helper = new OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
    override def onTick() = {}
  }

  def requestUnit[T <: Mobile](mobileType: Class[_ <: T], takeCareOfDependencies: Boolean) = {
    val req = ResourceRequests.forUnit(universe.race, mobileType)
    val result = resources.request(req, mobileEmployer)
    result match {
      case suc: ResourceApprovalSuccess =>
        val unitReq = UnitJobRequests.newOfType(universe, mobileEmployer, mobileType, suc)
        info(s"Financing possible for $mobileType, requesting training")
        val result = unitManager.request(unitReq)
        if (takeCareOfDependencies) {
          if (result.hasMissingRequirements) {
            // do not forget to unlock the resources again
            resources.unlock_!(suc)
          }
          result.missingRequirements.foreach { requirement =>
            if (!unitManager.plannedToBuild(requirement)) {
              helper.requestBuilding(requirement, takeCareOfDependencies = false)
            }
          }
        }
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
        requestBuilding(cap.typeOfFactory, takeCareOfDependencies = true)
      }
    }
  }

  private def evaluateCapacities = {
    strategy.current.suggestProducers
  }

}

case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory], maximumSustainable: Int)
case class IdealUnitRatio[T <: Mobile](unitType: Class[_ <: Mobile], amount: Int) {
  def fixedAmount = amount max 1
}

class ProvideArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  override def onTick(): Unit = {
    val p = percentages
    val mostMissing = p.wanted.toVector.sortBy { case (t, idealRatio) =>
      val existingRatio = p.existing.getOrElse(t, 0.0)
      existingRatio - idealRatio
    }
    mostMissing.headOption.foreach { case (thisOne, _) =>
      requestUnit(thisOne, takeCareOfDependencies = true)
    }
  }
  def percentages = {
    val ratios = strategy.current.suggestUnits
    val summed = ratios.groupBy(_.unitType)
                 .map { case (t, v) =>
                   (t, v.map(_.fixedAmount).sum)
                 }
    val totalWanted = summed.values.sum
    val percentagesWanted = summed.map { case (t, v) => t -> v.toDouble / totalWanted }

    val existingCounts = {
      val existing = unitManager.unitsByType[Mobile].groupBy(_.getClass)
      summed.keySet.map { t =>
        t -> existing.get(t).map(_.size).getOrElse(0)
      }.toMap
    }

    val totalExisting = existingCounts.values.sum
    val percentagesExisting = existingCounts
                              .map { case (t, v) => t -> (if (totalExisting == 0) 0 else v.toDouble / totalExisting) }
    Percentages(percentagesWanted, percentagesExisting)
  }
  case class Percentages(wanted: Map[Class[_ <: Mobile], Double], existing: Map[Class[_ <: Mobile], Double])
}

object Strategy {

  trait LongTermStrategy extends HasUniverse {
    def suggestUnits: Seq[IdealUnitRatio[_ <: Mobile]]
    def suggestProducers: Seq[IdealProducerCount[_ <: UnitFactory]]
    def determineScore: Int
  }
  class Strategies(override val universe: Universe) extends HasUniverse {
    private val available              = new TerranHeavyMetal(universe) ::
                                         new TerranAirSuperiority(universe) ::
                                         new TerranHeavyAir(universe) ::
                                         new TerranFootSoldiers(universe) ::
                                         Nil
    private var best: LongTermStrategy = new IdleAround(universe)
    def current = best
    def tick(): Unit = {
      ifNth(121) {
        best = available.maxBy(_.determineScore)
      }
    }
  }
  class IdleAround(override val universe: Universe) extends LongTermStrategy {
    override def determineScore = -1
    override def suggestProducers = Nil
    override def suggestUnits = Nil
  }

  class TerranHeavyMetal(override val universe: Universe) extends LongTermStrategy {
    override def determineScore: Int = 50
    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases) ::
      IdealProducerCount(classOf[Factory], myBases * 3) ::
      IdealProducerCount(classOf[Starport], myBases) ::
      Nil
    }
    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3) ::
      IdealUnitRatio(classOf[Medic], 1) ::
      IdealUnitRatio(classOf[Ghost], 1) ::
      IdealUnitRatio(classOf[Vulture], 3) ::
      IdealUnitRatio(classOf[Tank], 5) ::
      IdealUnitRatio(classOf[Goliath], 3) ::
      IdealUnitRatio(classOf[ScienceVessel], 1) ::
      Nil
    }
  }

  class TerranHeavyAir(override val universe: Universe) extends TerranAirSuperiority(universe) {
    override def suggestUnits: List[IdealUnitRatio[Nothing]] = {
      IdealUnitRatio(classOf[ScienceVessel], 3) ::
      IdealUnitRatio(classOf[Battlecruiser], 10) ::
      super.suggestUnits
    }
    override def determineScore: Int = {
      super.determineScore + bases.rich.ifElse(10, -10)
    }
  }

  class TerranAirSuperiority(override val universe: Universe) extends LongTermStrategy {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3) ::
      IdealUnitRatio(classOf[Medic], 1) ::
      IdealUnitRatio(classOf[Ghost], 1) ::
      IdealUnitRatio(classOf[Vulture], 3) ::
      IdealUnitRatio(classOf[Tank], 1) ::
      IdealUnitRatio(classOf[Goliath], 1) ::
      IdealUnitRatio(classOf[Wraith], 5) ::
      IdealUnitRatio(classOf[Battlecruiser], 3) ::
      IdealUnitRatio(classOf[ScienceVessel], 2) ::
      Nil
    }

    override def determineScore: Int = mapLayers.isOnIsland(bases.mainBase.mainBuilding.tilePosition)
                                       .ifElse(100, 0)

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases) ::
      IdealProducerCount(classOf[Factory], myBases) ::
      IdealProducerCount(classOf[Starport], myBases * 3) ::
      Nil
    }
  }

  class TerranFootSoldiers(override val universe: Universe) extends LongTermStrategy {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 10) ::
      IdealUnitRatio(classOf[Firebat], 3) ::
      IdealUnitRatio(classOf[Medic], 4) ::
      IdealUnitRatio(classOf[Ghost], 2) ::
      IdealUnitRatio(classOf[Vulture], 1) ::
      IdealUnitRatio(classOf[Tank], 1) ::
      IdealUnitRatio(classOf[Goliath], 1) ::
      IdealUnitRatio(classOf[Wraith], 1) ::
      IdealUnitRatio(classOf[Battlecruiser], 1) ::
      IdealUnitRatio(classOf[ScienceVessel], 1) ::
      Nil
    }

    override def determineScore: Int = (mapLayers.rawWalkableMap.size <= 64 * 64).ifElse(100, 0)

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases * 5) ::
      IdealProducerCount(classOf[Factory], myBases) ::
      IdealProducerCount(classOf[Starport], myBases) ::
      Nil
    }

  }

}