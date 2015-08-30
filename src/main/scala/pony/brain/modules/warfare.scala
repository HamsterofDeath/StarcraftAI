package pony
package brain
package modules

trait AddonRequestHelper extends AIModule[CanBuildAddons] {
  self =>

  def requestAddon[T <: Addon](addonType: Class[_ <: T]): Unit = {
    val req = ResourceRequests.forUnit(universe.race, addonType, Priority.Addon)
    val result = resources.request(req, self)
    result.ifSuccess { suc =>
      val unitReq = UnitJobRequests.addonConstructor(self, addonType)
      debug(s"Financing possible for $addonType, requesting build")
      val result = unitManager.request(unitReq)
      if (result.hasAnyMissingRequirements || !result.success) {
        resources.unlock_!(suc)
      } else {
        result.ifOne { one =>
          assignJob_!(new ConstructAddon(self, one, addonType, suc))
        }
      }
    }
  }
}


trait AlternativeBuildingSpot {
  def isEmpty = !shouldUse

  def shouldUse: Boolean

  def evaluateCostly: Option[MapTilePosition]

  def predefined:Option[MapTilePosition]
}

object AlternativeBuildingSpot {
  def fromExpensive(evalPosition: => Option[MapTilePosition]): AlternativeBuildingSpot = new AlternativeBuildingSpot {
    override def shouldUse = true
    override def evaluateCostly = evalPosition
    override def predefined = None
  }

  def fromPreset(fixedPosition: MapTilePosition):AlternativeBuildingSpot = fromPreset(Some(fixedPosition))

  def fromPreset(fixedPosition: Option[MapTilePosition]): AlternativeBuildingSpot = new AlternativeBuildingSpot {
    override def shouldUse = true
    override def evaluateCostly = throw new RuntimeException("This should not be called")
    override def predefined = fixedPosition
  }

  val useDefault = new AlternativeBuildingSpot {
    override def evaluateCostly = None
    override def shouldUse = false
    override def predefined= None
  }
}


trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T],
                                     takeCareOfDependencies: Boolean = false,
                                     saveMoneyIfPoor:Boolean = false,
                                     customBuildingPosition: AlternativeBuildingSpot = AlternativeBuildingSpot.useDefault,
                                     belongsTo:Option[ResourceArea] = None): Unit = {
    val req = ResourceRequests.forUnit(universe.race, buildingType)
    val result = resources.request(req, buildingEmployer)
    result.ifSuccess { suc =>
        val unitReq = UnitJobRequests.newOfType(universe, buildingEmployer, buildingType, suc,
          customBuildingPosition = customBuildingPosition, belongsTo = belongsTo)
      debug(s"Financing possible for $buildingType, requesting build")
        val result = unitManager.request(unitReq)
        if (result.hasAnyMissingRequirements) {
          resources.unlock_!(suc)
        }
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { what =>
            if (!unitManager.plannedToBuild(what)) {
              assert(customBuildingPosition.isEmpty, s"Does not make sense...")
              requestBuilding(what, takeCareOfDependencies, saveMoneyIfPoor, customBuildingPosition, belongsTo)
            }
          }
        }
    }
    if (result.failed && saveMoneyIfPoor) {
      resources.forceLock_!(req, buildingEmployer)
    }
  }
}

trait UnitRequestHelper extends AIModule[UnitFactory] {
  private val mobileEmployer = new Employer[Mobile](universe)

  private val buildingHelper = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val addonHelper    = new HelperAIModule[CanBuildAddons](universe) with AddonRequestHelper

  def requestUnit[T <: Mobile](mobileType: Class[_ <: T], takeCareOfDependencies: Boolean) = {
    val req = ResourceRequests.forUnit(universe.race, mobileType)
    val result = resources.request(req, mobileEmployer)
    result.ifSuccess { suc =>
        val unitReq = UnitJobRequests.newOfType(universe, mobileEmployer, mobileType, suc)
      debug(s"Financing possible for $mobileType, requesting training")
        val result = unitManager.request(unitReq)
        if (result.hasAnyMissingRequirements) {
          // do not forget to unlock the resources again
          resources.unlock_!(suc)
        }
        if (takeCareOfDependencies) {
          result.notExistingMissingRequiments.foreach { requirement =>
            if (!unitManager.plannedToBuild(requirement)) {
              def isAddon = classOf[Addon].isAssignableFrom(requirement)
              if (isAddon) {
                addonHelper.requestAddon(requirement.asInstanceOf[Class[_ <: Addon]])
              } else {
                buildingHelper.requestBuilding(requirement, takeCareOfDependencies = false)
              }
            }
          }
        }
    }
  }
}

trait UpgradePrice {

  def forUpgrade: Upgrade

  def nextMineralPrice: Int

  def nextGasPrice: Int

}

class ProvideSuggestedAddons(universe: Universe)
  extends OrderlessAIModule[CanBuildAddons](universe) with AddonRequestHelper {

  override def onTick(): Unit = {
    val buildUs = strategy.current.suggestAddons
                  .filter(_.isActive)
    val todo = for (builder <- units.allAddonBuilders;
                    addon <- buildUs
                    if builder.canBuildAddon(addon.addon) & !builder.hasAddonAttached) yield (builder, addon)
    todo.foreach { case (builder, what) =>
      requestAddon(what.addon)
    }
  }
}

class ProvideUpgrades(universe: Universe) extends OrderlessAIModule[Upgrader](universe) {
  self =>
  private val helper     = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val researched = collection.mutable.Map.empty[Upgrade, Int]

  override def onTick(): Unit = {
    strategy.current.suggestUpgrades
    .filterNot(e => researched.contains(e.upgrade))
    .filter(_.isActive)
    .foreach { request =>
      val wantedUpgrade = request.upgrade
      val needs = race.techTree.upgraderFor(wantedUpgrade)
      val patienceRequired = unitManager.existsOrPlanned(needs)
      if (!patienceRequired) {
        helper.requestBuilding(needs, takeCareOfDependencies = true)
      } else {
        val result = unitManager.request(UnitJobRequests.upgraderFor(wantedUpgrade, self))
        result.units.foreach { up =>
          val price = new UpgradePrice {
            private val current = researched.getOrElse(wantedUpgrade, 0)
            override def nextMineralPrice = wantedUpgrade.mineralPriceForStep(current)
            override def forUpgrade = wantedUpgrade
            override def nextGasPrice = wantedUpgrade.gasPriceForStep(current)
          }
          val result = resources.request(ResourceRequests.forUpgrade(up, price), self)
          result.ifSuccess { app =>
            info(s"Starting research of $wantedUpgrade")
            val researchUpgrade = new ResearchUpgrade(self, up, wantedUpgrade, app)
            researchUpgrade.listen_!(() => {
              info(s"Research of $wantedUpgrade completed")
              val current = researched.getOrElse(wantedUpgrade, 0)
              researched.put(wantedUpgrade, current + 1)
              upgrades.notifyResearched_!(wantedUpgrade)
            })
            assignJob_!(researchUpgrade)
          }
        }
      }
    }
  }
}

class EnqueueFactories(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

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

class EnqueueArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

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
    def suggestNextExpansion:Option[ResourceArea]
    def suggestUpgrades: Seq[UpgradeToResearch]
    def suggestUnits: Seq[IdealUnitRatio[_ <: Mobile]]
    def suggestProducers: Seq[IdealProducerCount[_ <: UnitFactory]]
    def suggestAddons: Seq[AddonToAdd] = Nil
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
        def isSinceVeryEarlyMid = time.minutes >= 4
        def isSinceEarlyMid = time.minutes >= 5
        def isSinceMid = time.minutes >= 8
        def isSincePostMid = time.minutes >= 9
        def isSinceLateMid = time.minutes >= 13
        def isBeforeLate = time.minutes <= 20

        def isAnyTime = true
      }
    }
  }

  trait TerranDefaults extends LongTermStrategy {
    override def suggestAddons: Seq[AddonToAdd] = {
      AddonToAdd(classOf[Comsat], requestNewBuildings = false)(unitManager.existsAndDone(classOf[Academy])) ::
      Nil
    }
    override def suggestNextExpansion = {
      def shouldExpand = expandNow
      if (shouldExpand) {
        val covered = bases.bases.map(_.resourceArea).toSet
        val where = bases.mainBase.mainBuilding.tilePosition
        val (choke, what) = strategicMap.domainsButWithout(covered)
                            .minBy(_._1.center.distanceToSquared(where))
        val target = what.values.flatten.minBy(_.patches.map(_.center.distanceToSquared(where)).getOrElse(999999))
        Some(target)
      } else {
        None
      }
    }
    protected def expandNow = {
      bases.myMineralFields.forall(_.remainingPercentage < expansionThreshold)
    }
    protected def expansionThreshold = 0.5
  }

  case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
    def isActive = active
  }
  case class AddonToAdd(addon: Class[_ <: Addon], requestNewBuildings: Boolean)(active: => Boolean) {
    def isActive = active
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
    override def suggestUpgrades = Nil
    override def suggestNextExpansion = None
  }

  class TerranHeavyMetal(override val universe: Universe) extends LongTermStrategy with TerranDefaults {
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
    override def suggestUpgrades: Seq[UpgradeToResearch] =
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceMid) ::
      Nil
  }

  class TerranHeavyAir(override val universe: Universe) extends TerranAirSuperiority(universe) with TerranDefaults {
    override def suggestUnits: List[IdealUnitRatio[Nothing]] = {
      IdealUnitRatio(classOf[ScienceVessel], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Battlecruiser], 10)(timingHelpers.phase.isAnyTime) ::
      super.suggestUnits
    }
    override def determineScore: Int = {
      super.determineScore + bases.rich.ifElse(10, -10)
    }

    override def suggestUpgrades = UpgradeToResearch(Upgrades.Terran.Defensematrix)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
                                   super.suggestUpgrades

  }

  class TerranAirSuperiority(override val universe: Universe) extends LongTermStrategy with TerranDefaults {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isMid) ::
      IdealUnitRatio(classOf[Vulture], 3)(timingHelpers.phase.isBeforeLate) ::
      IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 8)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 3)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 2)(timingHelpers.phase.isSincePostMid) ::
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
    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.WraithCloak)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.WraithEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.Defensematrix)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.MedicHeal)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(timingHelpers.phase.isAnyTime) ::
      Nil
  }

  class TerranFootSoldiers(override val universe: Universe) extends LongTermStrategy with TerranDefaults {

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(timingHelpers.phase.isSinceVeryEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(timingHelpers.phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceMid) ::
      Nil

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 15)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Firebat], 3)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Medic], 4)(timingHelpers.phase.isSinceVeryEarlyMid) ::
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
      IdealProducerCount(classOf[Barracks], myBases * 2)(timingHelpers.phase.isSincePostMid) ::
      IdealProducerCount(classOf[Factory], myBases)(timingHelpers.phase.isSinceLateMid) ::
      IdealProducerCount(classOf[Starport], myBases)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }

  }

}