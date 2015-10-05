package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FormationHelper(override val universe: Universe, distance: Int = 0) extends HasUniverse {
  def allOutsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator.flatMap(_.pointsOutside).filterNot(blacklisted).filter(map.free)
  }

  def allInsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator.flatMap(_.pointsInside).filterNot(blacklisted).filter(map.free)
  }

  def blacklisted(e: MapTilePosition) = blocked.contains(e)

  def cleanBlacklist(dispose: (MapTilePosition, BlacklistReason) => Boolean) = {
    blocked.filter(e => dispose(e._1, e._2))
    .foreach(e => whiteList_!(e._1))
  }

  private val myDefenseLines = LazyVal.from {
    bases.bases.flatMap { base =>
      strategicMap.defenseLineOf(base).map(_.tileDistance(distance))
    }
  }

  universe.bases.register((base: Base) => {
    myDefenseLines.invalidate()
  }, true)

  def defenseLines = myDefenseLines.get

  def blackList_!(tile: MapTilePosition): Unit = {
    blocked.put(tile, BlacklistReason(universe.currentTick))
  }

  def whiteList_!(tilePosition: MapTilePosition): Unit = {
    blocked.remove(tilePosition)
  }

  def reasonForBlacklisting(tilePosition: MapTilePosition) = blocked.get(tilePosition)

  private val blocked = mutable.HashMap.empty[MapTilePosition, BlacklistReason]

  case class BlacklistReason(when: Int)

}

class WorldDominationPlan(override val universe: Universe) extends HasUniverse with HasLazyVals {

  def allAttacks = attacks.toVector

  trait Action {
    def asOrder: UnitOrder
  }

  case class MoveToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.Move(who, where)
  }

  case class AttackToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.AttackMove(who, where)
  }

  case class StayInPosition(who: Mobile) extends Action {
    override def asOrder = Orders.NoUpdate(who)
  }

  class Attack(private var currentForce: Set[Mobile], where: TargetPosition) {
    assert(currentForce.nonEmpty, "WTF?")

    def hasMembers = currentForce.nonEmpty

    def hasEnded = !hasMembers || allReachedDestination
    def hasNotEnded = !hasEnded

    def allReachedDestination = migration.result.exists(_.allReachedDestination)

    def onTick(): Unit = {
      currentForce = currentForce.filter(_.isInGame)
    }

    def force = currentForce

    private val centerOfForce = oncePerTick {
      currentForce.foldLeft(MapTilePosition.shared(0, 0))((acc, e) =>
        acc.movedByNew(e.currentTile)
      ) / currentForce.size
    }

    def currentCenter = centerOfForce.get

    private val pathToFollow = universe.pathFinder.findPath(currentCenter, where.where)

    def completePath = pathToFollow

    private val migration = pathToFollow.map(_.map(new MigrationPath(_)))

    def suggestActionFor(t: Mobile) = {
      migration.result match {
        case None =>
          StayInPosition(t)
        case Some(path) =>
          path.nextFor(t) match {
            case Some((targetTile, attackMove)) =>
              if (attackMove) {
                AttackToPosition(t, targetTile)
              } else {
                MoveToPosition(t, targetTile)
              }
            case None =>
              StayInPosition(t)
          }
      }
    }
  }

  private val attacks = ArrayBuffer.empty[Attack]

  private           var planInProgress: BWFuture[Option[Attacks]] = BWFuture(None)
  @volatile private var thinking                                  = false

  override def onTick(): Unit = {
    super.onTick()
    attacks.foreach(_.onTick())
    attacks.retain(_.hasNotEnded)
    if (thinking) {
      planInProgress.result.foreach { plan =>
        attacks ++= plan.parts
        thinking = false
        planInProgress = BWFuture.none
        majorInfo(s"Attack plan finished!")
      }
    }
  }

  def attackOf(m: Mobile) = attacks.find(_.force(m))

  def initiateAttack(where: MapTilePosition, complete: Boolean = true): Unit = {
    majorInfo(s"Initiating attack of $where")
    assert(!thinking, s"Cannot multitask, sorry")
    //this attack has priority over an existing one
    if (complete) {
      attacks.clear()
    }

    val employer = new Employer[Mobile](universe)
    val req = UnitJobRequests.idleOfType(employer, classOf[Mobile], 9999)
    val result = unitManager.request(req, buildIfNoneAvailable = false)
    result.ifNotZero { seq =>
      val independent = seq.map { u => u.nativeUnitId -> u.currentTile }
      val helper = new GroupingHelper(universe)
      planInProgress = BWFuture.some {
        val on = universe.mapLayers.rawWalkableMap
        assert(independent.forall(e => on.includes(e._2)),
          s"""Unit alive but outside map?
              |${
            val ids = independent.filterNot(e => on.includes(e._2)).map(_._1)
            val problem = ids.map(universe.myUnits.byIdExpectExisting)
            problem.mkString("\n")
          }
              |""".stripMargin)

        val grouped = helper.groupUnits(independent)
        val newAttacks = grouped.map { group =>
          val asUnits = group.memberIds.map(e => ownUnits.byId(e).getOr(s"Id $e missing").asInstanceOf[Mobile])
          new Attack(asUnits.toSet, TargetPosition(where, 10))
        }
        Attacks(newAttacks)
      }
      thinking = true
    }
  }

  case class Attacks(parts: Seq[Attack])
}

class GroupingHelper(override val universe: Universe) extends HasUniverse {
  private val maxDst = 10 * 10
  class Group {

    def memberIds = members.iterator.map(_._1)

    def evalCenter = {
      var x = 0
      var y = 0
      members.foreach { case (_, p) =>
        x += p.x
        y += p.y
      }
      x /= members.size
      y /= members.size
      MapTilePosition.shared(x, y)
    }

    def add_!(elem: (Int, MapTilePosition)): Unit = {
      members += elem
      center = evalCenter
    }

    private val members = ArrayBuffer.empty[((Int, MapTilePosition))]
    private var center  = MapTilePosition.zero
    def canJoin(e: (Int, MapTilePosition)) = {
      e._2.distanceToSquared(center) < maxDst && map.connectedByLine(center, e._2)
    }
  }

  def groupUnits(independent: Seq[(Int, MapTilePosition)]) = {
    val groups = ArrayBuffer.empty[Group]
    independent.foreach { elem =>
      groups.find(_.canJoin(elem)) match {
        case Some(joinMe) => joinMe.add_!(elem)
        case None =>
          val ng = new Group
          groups += ng
          ng.add_!(elem)
      }
    }
    groups.toSeq
  }

  private val map = universe.mapLayers.rawWalkableMap.asReadOnlyCopyIfMutable
}

trait AddonRequestHelper extends AIModule[CanBuildAddons] {
  self =>

  private val helper = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper

  def requestAddon[T <: Addon](addonType: Class[_ <: T], handleDependencies: Boolean = false): Unit = {
    val req = ResourceRequests.forUnit(universe.myRace, addonType, Priority.Addon)
    val result = resources.request(req, self)
    result.ifSuccess { suc =>
      val unitReq = UnitJobRequests.addonConstructor(self, addonType)
      trace(s"Financing possible for $addonType, requesting build")
      val result = unitManager.request(unitReq)
      if (handleDependencies && result.hasAnyMissingRequirements) {
        result.notExistingMissingRequiments.foreach { what =>
          helper.requestBuilding(what, handleDependencies)
        }
        resources.unlock_!(suc)
      } else if (!result.success) {
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
  def init_!(): Unit
  def isEmpty = !shouldUse

  def shouldUse: Boolean

  def evaluateCostly: Option[MapTilePosition]

  def predefined: Option[MapTilePosition]
}

object AlternativeBuildingSpot {
  def fromExpensive[X](initOnMainThread: => X)
                      (evalPosition: (X) => Option[MapTilePosition]): AlternativeBuildingSpot = new
      AlternativeBuildingSpot {
    private var initPackage: X = _
    override def init_!(): Unit = {
      initPackage = initOnMainThread
    }

    override def shouldUse = true
    override def evaluateCostly = evalPosition(initPackage)
    override def predefined = None
  }

  def fromPreset(fixedPosition: MapTilePosition): AlternativeBuildingSpot = fromPreset(Some(fixedPosition))

  def fromPreset(fixedPosition: Option[MapTilePosition]): AlternativeBuildingSpot = new AlternativeBuildingSpot {
    override def shouldUse = true
    override def evaluateCostly = throw new RuntimeException("This should not be called")
    override def predefined = fixedPosition
    override def init_!(): Unit = {}
  }

  val useDefault = new AlternativeBuildingSpot {
    override def evaluateCostly = None
    override def shouldUse = false
    override def predefined = None
    override def init_!(): Unit = {}
  }
}

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T],
                                     takeCareOfDependencies: Boolean = false,
                                     saveMoneyIfPoor: Boolean = false,
                                     customBuildingPosition: AlternativeBuildingSpot = AlternativeBuildingSpot
                                                                                       .useDefault,
                                     belongsTo: Option[ResourceArea] = None,
                                     priority: Priority = Priority.Default): Unit = {
    val req = ResourceRequests.forUnit(universe.myRace, buildingType, priority)
    val result = resources.request(req, buildingEmployer)
    result.ifSuccess { suc =>
      val unitReq = UnitJobRequests.newOfType(universe, buildingEmployer, buildingType, suc,
        customBuildingPosition = customBuildingPosition, belongsTo = belongsTo,
        priority = priority)
      trace(s"Financing possible for $buildingType, requesting build")
      val result = unitManager.request(unitReq)
      if (result.hasAnyMissingRequirements) {
        resources.unlock_!(suc)
      }
      if (takeCareOfDependencies) {
        result.notExistingMissingRequiments.foreach { what =>
          if (!unitManager.plannedToBuild(what)) {
            assert(customBuildingPosition.isEmpty, s"Does not make sense...")
            requestBuilding(what, takeCareOfDependencies, saveMoneyIfPoor, customBuildingPosition, belongsTo,
              priority = priority)
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
    val req = ResourceRequests.forUnit(universe.myRace, mobileType)
    val result = resources.request(req, mobileEmployer)
    result.ifSuccess { suc =>
      val unitReq = UnitJobRequests.newOfType(universe, mobileEmployer, mobileType, suc)
      trace(s"Financing possible for $mobileType, requesting training")
      val result = unitManager.request(unitReq)
      if (result.hasAnyMissingRequirements) {
        // do not forget to unlock the resources again
        resources.unlock_!(suc)
      }
      if (takeCareOfDependencies) {
        result.notExistingMissingRequiments.foreach { requirement =>
          if (!unitManager.existsOrPlanned(requirement)) {
            def isAddon = classOf[Addon].isAssignableFrom(requirement)
            trace(s"Planning to build $requirement because it is required for $mobileType")
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
    val todo = (for (builder <- ownUnits.allAddonBuilders;
                     addon <- buildUs
                     if builder.canBuildAddon(addon.addon) & !builder.hasAddonAttached) yield (builder, addon))
               .toVector
    todo.foreach { case (builder, what) =>
      requestAddon(what.addon, what.requestNewBuildings)
    }
  }
}

class ProvideUpgrades(universe: Universe) extends OrderlessAIModule[Upgrader](universe) {
  self =>
  private val helper     = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val researched = collection.mutable.Map.empty[Upgrade, Int]

  private def hasLimitDisabler = universe.myUnits.allByType[UpgradeLimitLifter].nonEmpty

  override def onTick(): Unit = {
    val maxLimitEnabled = hasLimitDisabler
    strategy.current.suggestUpgrades
    .filterNot(e => researched.getOrElse(e.upgrade, 0) == maxLimitEnabled.ifElse(e.maxLevel, 1))
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
            researchUpgrade.listen_!(failed => {
              if (!failed) {
                info(s"Research of $wantedUpgrade completed")
                val current = researched.getOrElse(wantedUpgrade, 0)
                researched.put(wantedUpgrade, current + 1)
                upgrades.notifyResearched_!(wantedUpgrade)
              }
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
    val (canBuild, needsSomething) =
      mostMissing.partition { case (c, _) => universe.unitManager.allRequirementsFulfilled(c) }

    val canBuildNow = mostMissing.filterNot { case (c, _) => universe.unitManager.requirementsQueuedToBuild(c) }
    val highestPriority = canBuildNow
    highestPriority.view.takeWhile { e =>
      val rich = resources.couldAffordNow(e._1)
      def mineralsOverflow = resources.unlockedResources.moreMineralsThanGas &&
                             resources.unlockedResources.minerals > 400
      def gasOverflow = resources.unlockedResources.moreGasThanMinerals &&
                        resources.unlockedResources.gas > 400
      rich || mineralsOverflow || gasOverflow
    }.foreach { case (thisOne, _) =>
      requestUnit(thisOne, takeCareOfDependencies = false)
    }

    needsSomething.foreach { case (thisOne, _) =>
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
    def suggestNextExpansion: Option[ResourceArea]
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
        def isSinceAlmostMid = time.minutes >= 7
        def isSinceMid = time.minutes >= 8
        def isSincePostMid = time.minutes >= 9
        def isSinceLateMid = time.minutes >= 13
        def isSinceVeryLateMid = time.minutes >= 16
        def isBeforeLate = time.minutes <= 20

        def isAnyTime = true
      }
    }
  }

  trait TerranDefaults extends LongTermStrategy {
    override def suggestAddons: Seq[AddonToAdd] = {
      AddonToAdd(classOf[Comsat], requestNewBuildings = true)(
        unitManager.existsAndDone(classOf[Academy]) || timingHelpers.phase.isSinceEarlyMid) ::
      AddonToAdd(classOf[MachineShop], requestNewBuildings = false)(unitManager.existsAndDone(classOf[Factory])) ::
      AddonToAdd(classOf[ControlTower], requestNewBuildings = false)(unitManager.existsAndDone(classOf[Starport])) ::
      Nil
    }

    override def suggestNextExpansion = {
      val shouldExpand = expandNow
      if (shouldExpand) {
        val covered = bases.bases.map(_.resourceArea).toSet
        val dangerous = strategicMap.resources.filter { res =>
          universe.unitGrid.allInRangeOf[Mobile](res.center, 10, friendly = false).nonEmpty
        }.toSet
        bases.mainBase.map(_.mainBuilding.tilePosition).flatMap { where =>
          val others = strategicMap.resources
                       .filterNot(covered)
                       .filterNot(dangerous)
                       .filter { where =>
                         universe.myUnits.allByType[TransporterUnit].nonEmpty ||
                         mapLayers.rawWalkableMap
                         .areInSameArea(where.center, bases.mainBase.get.mainBuilding.tilePosition)
                       }
          if (others.nonEmpty) {
            val target = others.maxBy(
              e => (e.mineralsAndGas, -e.patches.map(_.center.distanceToSquared(where)).getOrElse(999999)))
            Some(target)
          } else {
            None
          }
        }
      } else {
        None
      }
    }
    protected def expandNow = {
      val (poor, rich) = bases.myMineralFields.partition(_.remainingPercentage < expansionThreshold)
      poor.size > rich.size && rich.size <= 3
    }
    protected def expansionThreshold = 0.5
  }

  case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
    val maxLevel = upgrade.nativeType.fold(_.maxRepeats, _ => 1)

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
      val myBases = bases.myMineralFields.count(_.remainingPercentage > 0.25)

      IdealProducerCount(classOf[Barracks], (myBases / 2) max 1)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], myBases * 3)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(timingHelpers.phase.isSincePostMid) ::
      Nil
    }
    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isMid) ::
      IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isLateMid) ::
      IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Vulture], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Tank], 5)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Goliath], 3)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Dropship], 1)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }
    override def suggestUpgrades: Seq[UpgradeToResearch] =
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceAlmostMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(timingHelpers.phase.isSinceVeryLateMid) ::
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

    override def suggestUpgrades = UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
                                   UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
                                   super.suggestUpgrades

  }

  class TerranAirSuperiority(override val universe: Universe) extends LongTermStrategy with TerranDefaults {

    override def suggestUnits = {
      IdealUnitRatio(classOf[Marine], 3)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Medic], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Ghost], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Vulture], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Dropship], 1)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 8)(timingHelpers.phase.isSinceMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 3)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 2)(timingHelpers.phase.isSincePostMid) ::
      Nil
    }

    override def determineScore: Int = {
      bases.mainBase.map { mb =>
        mapLayers.isOnIsland(mb.mainBuilding.tilePosition)
        .ifElse(100, 0)
      }.getOrElse(0)
    }

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
      UpgradeToResearch(Upgrades.Terran.WraithEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicHeal)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(timingHelpers.phase.isSinceVeryLateMid) ::
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
      IdealUnitRatio(classOf[Medic], 4)(timingHelpers.phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Ghost], 2)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Vulture], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Tank], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Wraith], 1)(timingHelpers.phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 1)(timingHelpers.phase.isLate) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(timingHelpers.phase.isSinceLateMid) ::
      Nil
    }

    override def determineScore: Int = (mapLayers.rawWalkableMap.size <= 96 * 96).ifElse(100, 0)

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