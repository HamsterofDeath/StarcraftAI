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
        if (result.hasAnyMissingRequirements) {
          resources.unlock_!(suc)
        }
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { what =>
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
        if (result.hasAnyMissingRequirements) {
          // do not forget to unlock the resources again
          resources.unlock_!(suc)
        }
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { requirement =>
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
    .filter(_.isActive)
    .groupBy(_.typeOfFactory)
    .values.map { elems =>
      val sum = elems.map(_.maximumSustainable).sum
      val copy = IdealProducerCount(elems.head.typeOfFactory, sum)(true)
      copy
    }
  }

}

case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory], maximumSustainable: Int)
                                               (active: => Boolean) {
  def isActive = active
}
case class IdealUnitRatio[T <: Mobile](unitType: Class[_ <: Mobile], amount: Int)(active: => Boolean) {
  def fixedAmount = amount max 1

  def isActive = active
}

class ProvideArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  override def onTick(): Unit = {
    val p = percentages
    val mostMissing = p.wanted.toVector.sortBy { case (t, idealRatio) =>
      val existingRatio = p.existing.getOrElse(t, 0.0)
      existingRatio - idealRatio
    }
    mostMissing.foreach { case (thisOne, _) =>
      requestUnit(thisOne, takeCareOfDependencies = true)
    }
  }
  def percentages = {
    val ratios = strategy.current.suggestUnits.filter(_.isActive)
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
    val timingHelpers = new TimingHelpers
    def suggestUnits: Seq[IdealUnitRatio[_ <: Mobile]]
    def suggestProducers: Seq[IdealProducerCount[_ <: UnitFactory]]
    def determineScore: Int
    class TimingHelpers {

      def phase = new Phase
      class Phase {
        def isLate = isBetween(20, 9999)
        def isLateMid = isBetween(13, 20)
        def isMid = isBetween(9, 13)
        def isBetween(from: Int, to: Int) = time.minutes >= from && time.minutes < to
        def isEarlyMid = isBetween(5, 9)
        def isEarly = isBetween(0, 5)
        def isSinceAlmostMid = time.minutes >= 4
        def isSinceEarlyMid = time.minutes >= 5
        def isSinceMid = time.minutes >= 9
        def isSinceLateMid = time.minutes >= 13

        def isBeforeLate = time.minutes <= 20

        def isAnyTime = true
      }
    }
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

      IdealProducerCount(classOf[Barracks], myBases)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], myBases * 3)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], myBases)(timingHelpers.phase.isAnyTime) ::
      Nil
    }
    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Vulture], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Tank], 5)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Goliath], 3)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }
  }

  class TerranHeavyAir(override val universe: Universe) extends TerranAirSuperiority(universe) {
    override def suggestUnits: List[IdealUnitRatio[Nothing]] = {
      IdealUnitRatio(classOf[ScienceVessel], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Battlecruiser], 10)(timingHelpers.phase.isAnyTime) ::
      super.suggestUnits
    }
    override def determineScore: Int = {
      super.determineScore + bases.rich.ifElse(10, -10)
    }
  }

  class TerranAirSuperiority(override val universe: Universe) extends LongTermStrategy {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isMid) ::
      IdealUnitRatio(classOf[Vulture], 3)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Wraith], 8)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 3)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 2)(timingHelpers.phase.isSinceMid) ::
      Nil
    }

    override def determineScore: Int = mapLayers.isOnIsland(bases.mainBase.mainBuilding.tilePosition)
                                       .ifElse(100, 0)

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], myBases)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], myBases * 3)(timingHelpers.phase.isAnyTime) ::
      Nil
    }
  }

  class TerranFootSoldiers(override val universe: Universe) extends LongTermStrategy {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 10)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Firebat], 3)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Medic], 4)(timingHelpers.phase.isSinceAlmostMid) ::
      IdealUnitRatio(classOf[Ghost], 2)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Vulture], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Wraith], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 1)(timingHelpers.phase.isLate) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }

    override def determineScore: Int = (mapLayers.rawWalkableMap.size <= 64 * 64).ifElse(100, 0)

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.value > 1000)

      IdealProducerCount(classOf[Barracks], myBases * 3)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Barracks], myBases * 2)(timingHelpers.phase.isSinceMid) ::
      IdealProducerCount(classOf[Factory], myBases)(timingHelpers.phase.isSinceLateMid) ::
      IdealProducerCount(classOf[Starport], myBases)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }

  }

}