package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FormationHelper(override val universe: Universe,
                      paths: Paths,
                      distanceForUnits: Int = 1,
                      isGroundPath: Boolean) extends HasUniverse {
  private val target             = paths.anyTarget
  private val assignments        = mutable.HashMap.empty[Mobile, MapTilePosition]
  private val used               = mutable.HashSet.empty[MapTilePosition]
  private val availablePositions = {
    val walkable = mapLayers.rawWalkableMap.guaranteeImmutability

    BWFuture.from {
      val minDst = 24
      if (isGroundPath) {
        val availableArea = {
          mapLayers.safeGround.mutableCopy
        }

        val targetArea = walkable.areaOf(target).getOr(s"No area contains $target")
        val validTiles = availableArea.spiralAround(target, 80)
                         .filter(availableArea.freeAndInBounds)
                         .filter(targetArea.freeAndInBounds)
                         .toVector

        val unsorted = validTiles.filter { p => paths.isEmpty || paths.minimalDistanceTo(p) < 10 }

        unsorted.sortBy(_.distanceSquaredTo(target))
      } else {
        val availableArea = {
          mapLayers.safeAir.mutableCopy
        }
        val validTiles = availableArea.spiralAround(target, 80)
                         .filter(availableArea.freeAndInBounds)
                         .toVector

        val unsorted = validTiles.filter { p => paths.isEmpty || paths.minimalDistanceTo(p) < 10 }

        unsorted.sortBy(_.distanceSquaredTo(target))
      }
    }
  }

  def formationTiles = assignments.valuesIterator

  def assignedPosition(mobile: Mobile) = {
    assignments.get(mobile).orElse {
      availablePositions.matchOnOptSelf(vec => {
        val hereOpt = vec.find(e => !used(e))
        hereOpt match {
          case Some(here) =>
            assignments.put(mobile, here)
            used += here

            hereOpt
          case _ =>
            trace(s"No open slot for $mobile")

            None
        }
      }, Option.empty)
    }
  }
}

class FormationAtFrontLineHelper(override val universe: Universe, distance: Int = 0)
  extends HasUniverse {
  private val myDefenseLines = LazyVal.from {
    bases.bases.flatMap { base =>
      strategicMap.defenseLineOf(base).map(_.tileDistance(distance))
    }
  }

  private val blocked = mutable.HashMap.empty[MapTilePosition, BlacklistReason]

  def allOutsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator
    .flatMap { e =>
      e.pointsOutside
      .filterNot(blacklisted)
      .filter(map.free)
    }
  }

  def blacklisted(e: MapTilePosition) = blocked.contains(e)

  def defenseLines = myDefenseLines.get

  universe.bases.register((base: Base) => {
    myDefenseLines.invalidate()
  }, notifyForExisting = true)

  def allInsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator.flatMap(_.pointsInside).filterNot(blacklisted).filter(map.free)
  }

  def cleanBlacklist(dispose: (MapTilePosition, BlacklistReason) => Boolean) = {
    blocked.filter(e => dispose(e._1, e._2))
    .foreach(e => whiteList_!(e._1))
  }

  def whiteList_!(tilePosition: MapTilePosition): Unit = {
    blocked.remove(tilePosition)
  }

  def blackList_!(tile: MapTilePosition): Unit = {
    blocked.put(tile, BlacklistReason(universe.currentTick))
  }

  def reasonForBlacklisting(tilePosition: MapTilePosition) = blocked.get(tilePosition)

  case class BlacklistReason(when: Int)

}

class WorldDominationPlan(override val universe: Universe) extends HasUniverse {

  private           val attacks                                             = ArrayBuffer.empty[Attack]
  private           var planInProgress: BWFuture[Option[IncompleteAttacks]] = BWFuture(None)
  @volatile private var thinking                                            = false

  def allAttacks = attacks.toVector

  override def onTick_!(): Unit = {
    super.onTick_!()
    attacks.foreach(_.onTick())
    attacks.retain(_.hasNotEnded)
    if (thinking) {
      planInProgress.result.foreach { plan =>
        attacks ++= plan.complete.parts
        thinking = false
        planInProgress = BWFuture.none
        majorInfo(s"Attack plan finished!")
      }
    }
  }

  def attackOf(m: Mobile) = attacks.find(_.force(m))

  def initiateAttack(where: MapTilePosition, complete: Boolean = true): Unit = {
    majorInfo(s"Initiating attack of $where")
    assert(!thinking, s"Cannot multitask yet, sorry")
    //this attack has priority over an existing one
    if (complete) {
      attacks.clear()
    }

    val employer = new Employer[Mobile](universe)
    val req = UnitJobRequest.idleOfType(employer, classOf[Mobile], 9999)
    val result = unitManager.request(req, buildIfNoneAvailable = false)
    result.ifNotZero { seq =>
      initiateAttack(where, seq)
    }
  }

  def initiateAttack(where: MapTilePosition, units: Seq[Mobile]): Unit = {
    debug(s"Attacking $where with $units")
    val helper = new GroupingHelper(universe.mapLayers.rawWalkableMap, units, universe.allUnits)
    planInProgress = BWFuture.produceFrom {
      val on = universe.mapLayers.rawWalkableMap
      val grouped = helper.evaluateUnitGroups
      val newAttacks = grouped.map { group =>
        val asUnits = group.memberIds
                      .flatMap { e =>
                        val op = ownUnits.byId(e)
                        op.forNone {
                          warn(s"Cannot find unit with id $e aka ${e.toBase36}")
                        }
                        op.map(_.asInstanceOf[Mobile])
                      }


        new IncompleteAttack(asUnits.toSet, TargetPosition(where, 10))
      }
      debug(s"Attack calculation finished, results: $newAttacks")
      IncompleteAttacks(newAttacks)
    }
    thinking = true
  }

  trait Action {
    def asOrder: UnitOrder
  }

  case class MoveToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.MoveToTile(who, where)
  }

  case class AttackToPosition(who: Mobile, where: MapTilePosition) extends Action {
    override def asOrder = Orders.AttackMove(who, where)
  }

  case class StayInPosition(who: Mobile) extends Action {
    override def asOrder = Orders.NoUpdate(who)
  }

  class IncompleteAttack(private var currentForce: Set[Mobile], meetingPoint: TargetPosition) {
    def complete = new Attack(currentForce, meetingPoint)
  }

  class Attack(private var currentForce: Set[Mobile], meetingPoint: TargetPosition) {
    private val area = {
      currentForce.map { unit =>
        mapLayers.rawWalkableMap
        .areaOf(unit.currentTile)
      }
      .groupBy(identity)
      .mapValuesStrict(_.size)
      .maxBy(_._2)
      ._1
    }

    assert(currentForce.nonEmpty, "WTF?")
    private val centerOfForce = oncePerTick {
      val realCenter = {
        currentForce.foldLeft(MapTilePosition.shared(0, 0))((acc, e) =>
          acc.movedByNew(e.currentTile)
        ) / currentForce.size
      }
      area.flatMap(_.nearestFree(realCenter))
      .getOrElse(realCenter)
    }
    private val pathToFollow = universe.pathfinders.groundSafe
                               .findPaths(currentCenter, meetingPoint.where)
    private val migration    = pathToFollow.map(_.map(_.toMigration))

    def destination = meetingPoint

    def migrationPlan = migration.result

    def hasNotEnded = !hasEnded

    def hasEnded = !hasMembers || allReachedDestination

    def hasMembers = currentForce.nonEmpty

    def allReachedDestination = migration.result.exists(_.allReachedDestination)

    def onTick(): Unit = {
      currentForce = currentForce.filter(_.isInGame)
    }

    def force = currentForce

    def currentCenter = centerOfForce.get

    def completePath = pathToFollow

    def suggestActionFor(t: Mobile) = {
      migration.result match {
        case None =>
          // no path calculated yet
          StayInPosition(t)
        case Some(path) =>
          def defaultCommand = {
            path.nextFor(t) match {
              case Some((targetTile, attackMove)) if attackMove =>
                AttackToPosition(t, targetTile)
              case Some((targetTile, attackMove)) =>
                MoveToPosition(t, targetTile)
              case None =>
                StayInPosition(t)
            }
          }

          t match {
            case s: SupportUnit =>
              val stayHere = {
                val stayBetweenThese = s.nearestAllies.iterator
                                       .filter(_.currentTile.distanceToIsLess(s.currentTile, 8))
                                       .take(3)
                                       .map(_.currentTile)
                MapTilePosition.averageOpt(stayBetweenThese)
              }
              stayHere.map {AttackToPosition(t, _)}.getOrElse {defaultCommand}
            case _ =>
              defaultCommand
          }
      }
    }
  }

  case class Attacks(parts: Seq[Attack])

  case class IncompleteAttacks(parts: Seq[IncompleteAttack]) {
    def complete = Attacks(parts.map(_.complete))
  }


}

case class UnitGroup[T <: WrapsUnit](members: Seq[T], center: MapTilePosition)

object GroupingHelper {
  def typedGroup[T <: WrapsUnit](universe: Universe, group: Group[T]) = {
    val members = group.memberIds.flatMap { e =>
      universe.enemyUnits.byId(e).orElse(universe.ownUnits.byId(e)).asInstanceOf[Option[T]]
    }.toVector
    UnitGroup(members, group.center)
  }

  def groupThese[T <: WrapsUnit](seq: TraversableOnce[T], universe: Universe) = {
    val helper = new GroupingHelper(universe.mapLayers.rawWalkableMap.guaranteeImmutability, seq,
      universe.allUnits)
    BWFuture(Option(helper.evaluateUnitGroups))
  }

  def groupTheseNow[T <: WrapsUnit](seq: TraversableOnce[T], universe: Universe) = {
    val helper = new GroupingHelper(universe.mapLayers.rawWalkableMap.guaranteeImmutability, seq,
      universe.allUnits)
    helper.evaluateUnitGroups
  }

  def groupTheseNow[T <: WrapsUnit](seq: TraversableOnce[T], map: Grid2D, allUnits: AllUnits) = {
    val helper = new GroupingHelper(map.guaranteeImmutability, seq, allUnits)
    helper.evaluateUnitGroups
  }
}

class GroupingHelper[T <: WrapsUnit](val map: Grid2D, seq: TraversableOnce[T], source: AllUnits) {
  private val immutable = seq.map { u => u.nativeUnitId -> u.centerTile }.toVector

  /**
    * can/should be run asynchronously
    *
    * @return
    */
  def evaluateUnitGroups = {
    val groups = ArrayBuffer.empty[Group[T]]
    immutable.foreach { elem =>
      groups.find(_.canJoin(elem)) match {
        case Some(joinMe) => joinMe.add_!(elem)
        case None =>
          val ng = new Group[T](map, source)
          groups += ng
          ng.add_!(elem)
      }
    }
    groups.toSeq
  }

}

class Group[T <: WrapsUnit](map: Grid2D, source: AllUnits) {
  private val maxDst    = 10 * 10
  private val myMembers = ArrayBuffer.empty[((Int, MapTilePosition))]
  private var myCenter  = MapTilePosition.zero

  def memberUnits = {
    val typed = (memberIds.flatMap(source.own.byId) ++
                 memberIds.flatMap(source.other.byId)).toVector

    assert(typed.size == size, s"Expected $size but found only ${typed.size}")
    typed.asInstanceOf[Vector[T]]
  }

  def size = myMembers.size

  def memberIds = myMembers.iterator.map(_._1)

  def center = myCenter

  def add_!(elem: (Int, MapTilePosition)): Unit = {
    myMembers += elem
    myCenter = evalCenter
  }

  private def evalCenter = {
    var x = 0
    var y = 0
    myMembers.foreach { case (_, p) =>
      x += p.x
      y += p.y
    }
    x /= myMembers.size
    y /= myMembers.size
    MapTilePosition.shared(x, y)
  }

  def canJoin(e: (Int, MapTilePosition)) = {
    e._2.distanceSquaredTo(myCenter) < maxDst && map.connectedByLine(myCenter, e._2)
  }
}

trait AddonRequestHelper extends AIModule[CanBuildAddons] {
  self =>

  private val helper = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper

  def requestAddon[T <: Addon](addonType: Class[_ <: T],
                               handleDependencies: Boolean = false): Unit = {
    trace(s"Addon ${addonType.className} requested")
    val req = ResourceRequests.forUnit(universe.myRace, addonType, Priority.Addon)
    val result = resources.request(req, self)
    requestAddonIfResourcesProvided(addonType, handleDependencies, result)
  }

  def requestAddonIfResourcesProvided[T <: Addon](addonType: Class[_ <: T],
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

trait AlternativeBuildingSpot {
  def init_!(): Unit
  def isEmpty = !shouldUse

  def shouldUse: Boolean

  def evaluateCostly: Option[MapTilePosition]

  def predefined: Option[MapTilePosition]
}

object AlternativeBuildingSpot {
  val useDefault = new AlternativeBuildingSpot {
    override def evaluateCostly = None

    override def shouldUse = false

    override def predefined = None

    override def init_!(): Unit = {}
  }

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

  def fromPreset(fixedPosition: MapTilePosition): AlternativeBuildingSpot = fromPreset(
    Some(fixedPosition))

  def fromPreset(fixedPosition: Option[MapTilePosition]): AlternativeBuildingSpot = new
      AlternativeBuildingSpot {

    fixedPosition.foreach { where =>
      assert(where.x < 1000)
      assert(where.y < 1000)
    }

    override def shouldUse = true

    override def evaluateCostly = throw new RuntimeException("This should not be called")

    override def predefined = fixedPosition

    override def init_!(): Unit = {}
  }
}

trait BuildingRequestHelper extends AIModule[WorkerUnit] {
  private val buildingEmployer = new Employer[Building](universe)

  def requestBuilding[T <: Building](buildingType: Class[_ <: T],
                                     takeCareOfDependencies: Boolean = false,
                                     saveMoneyIfPoor: Boolean = false,
                                     customBuildingPosition: AlternativeBuildingSpot =
                                     AlternativeBuildingSpot
                                     .useDefault,
                                     belongsTo: Option[ResourceArea] = None,
                                     priority: Priority = Priority.Default): Unit = {
    val req = ResourceRequests.forUnit(universe.myRace, buildingType, priority)
    val result = resources.request(req, buildingEmployer)
    result.ifSuccess { suc =>
      val unitReq = UnitJobRequest.newOfType(universe, buildingEmployer, buildingType, suc,
        customBuildingPosition = customBuildingPosition, belongsTo = belongsTo,
        priority = priority)
      trace(s"Financing possible for building $buildingType, requesting build")
      val result = unitManager.request(unitReq)
      if (result.hasAnyMissingRequirements) {
        resources.unlock_!(suc)
      }
      if (takeCareOfDependencies) {
        result.notExistingMissingRequiments.foreach { what =>
          if (!unitManager.plannedToBuild(what)) {
            requestBuilding(what, takeCareOfDependencies, saveMoneyIfPoor,
              AlternativeBuildingSpot.useDefault, belongsTo, priority = priority)
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
      val unitReq = UnitJobRequest.newOfType(universe, mobileEmployer, mobileType, suc)
      trace(s"Financing possible for mobile unit $mobileType, requesting training")
      val result = unitManager.request(unitReq)
      if (result.hasAnyMissingRequirements) {
        // do not forget to unlock the resources again
        trace(s"Requirement missing for $mobileType, unlocking resource")
        resources.unlock_!(suc)
      }
      if (takeCareOfDependencies) {
        result.notExistingMissingRequiments.foreach { requirement =>
          trace(s"Checking dependency: $requirement")
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

class ProvideSuggestedAndRequestedAddons(universe: Universe)
  extends OrderlessAIModule[CanBuildAddons](universe) with AddonRequestHelper {

  override def onTick_!(): Unit = {
    val suggested = {
      val buildUs = strategy.current.suggestAddons
                    .filter(_.isActive)
      for (builder <- ownUnits.allAddonBuilders;
           addon <- buildUs
           if builder.canBuildAddon(addon.addon) & !builder.hasAddonAttached) yield (builder, addon)
    }

    suggested.filter(e => canBuildMoreOf(e._2.addon)).foreach { case (builder, what) =>
      requestAddon(what.addon, what.requestNewBuildings)
    }

    val requested = unitManager.failedToProvideByType[Addon].iterator.collect {
      case attachIt: BuildUnitRequest[Addon]
        if attachIt.proofForFunding.isSuccess &&
           universe.resources.hasStillLocked(attachIt.funding) =>
        attachIt
    }

    requested.filter { e =>
      val ok = canBuildMoreOf(e.typeOfRequestedUnit)
      if (!ok) {
        info(s"Prevented duplicate production of ${e.typeOfRequestedUnit} - fixme")
        e.forceUnlockOnDispose_!()
        e.dispose()
      }

      ok
    }.foreach { req =>
      req.clearableInNextTick_!()
      requestAddonIfResourcesProvided(req.typeOfRequestedUnit, handleDependencies = false,
        req.proofForFunding)
    }

  }

  private def canBuildMoreOf(addon: Class[_ <: Addon]) = {
    val existing = ownUnits.allByClass(addon)
    if (existing.isEmpty) {
      true
    } else {
      val any = existing.head
      def noUpgrades = !any.isInstanceOf[Upgrader]
      def requiredForUnit = race.techTree
                            .requiredBy.get(any.getClass)
                            .exists(_.exists(classOf[Mobile].isAssignableFrom))
      noUpgrades || requiredForUnit

    }
  }
}

class SetupAntiCloakDefenses(universe: Universe)
  extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  private val richBaseCount = oncePerTick {
    bases.richBasesCount
  }
  private val analyzed = FutureIterator.feed(input) produceAsyncLater { in =>

    val counts = mutable.HashMap.empty[MapTilePosition, Int]

    // goal: position detectors so that all values are >= 0
    in.exposed.allBlocked
    .filter(in.ownArea.blocked)
    .foreach { where =>
      val isIsland = mapLayers.isOnIsland(where)
      counts.put(where, -in.limit - (if (isIsland) 2 else 0))
    }

    in.existing.foreach { case (detector, circle) =>
      circle.asTiles.foreach { where =>
        if (counts.contains(where)) {
          counts.insertReplace(where, _ + 1 min 0, 0)
        }
      }
    }

    val exposure = -counts.values.sum

    if (exposure > 0) {
      // find the next spot that minimizes the exposure
      val byPriority = counts.keySet.toVector.sortBy { candidate =>
        val covered = {
          geoHelper.circle(candidate, 8).asTiles.count { tile =>
            counts.get(tile) match {
              case Some(value) if value < 0 => true
              case _ => false
            }
          }
        }
        exposure - covered
      }
      // of those, take the first one that is ok
      val bestPosition = byPriority.iterator.flatMap { where =>
        in.constructionSiteFinder.findSpotFor(where, in.buildingType, 1, 1)
      }.toStream.headOption
      info(s"Next detector should be built at ${bestPosition.get}", bestPosition.isDefined)
      trace(s"Exposed by $exposure, but cannot add detector building", bestPosition.isEmpty)
      bestPosition
    } else {
      None
    }
  }

  override def onTick_!() = {
    super.onTick_!()
    kickOffOn24thTick()
    val active = strategy.current.buildAntiCloakNow
    analyzed.mostRecent.foreach { bestBuildingLocation =>
      if (active) {
        val buildingInProgress = {
          unitManager.constructionsInProgress[DetectorBuilding].nonEmpty
        }
        def buildingExists = {
          bestBuildingLocation.exists { where =>
            ownUnits.buildingAt(where).isDefined
          }
        }
        def outdated = {
          analyzed.lastUsedFeed.map(_.age).getOrElse(0) > 80
        }

        if (buildingInProgress || outdated || buildingExists) {
          analyzed.prepareNextIfDone()
        } else {
          bestBuildingLocation.foreach { where =>
            requestBuilding(targetBuildingType, takeCareOfDependencies = true,
              customBuildingPosition = AlternativeBuildingSpot.fromPreset(where))
          }
        }
      }
    }
  }

  private def targetBuildingType = race.detectorBuildingClass

  private def kickOffOn24thTick(): Unit = {
    if (currentTick == 24) {
      analyzed.prepareNextIfDone()
    }
  }

  private def input = new Input

  class Input {
    val limit                  = richBaseCount.get min 3
    val created                = currentTick
    val buildingType           = targetBuildingType
    val existing               = ownUnits.allByClass(buildingType).map { det =>
      det -> det.detectionArea
    }
    val planned                = unitManager.plannedToBuildByClass(buildingType)
    val exposed                = mapLayers.exposedToCloakedUnits
    val ownArea                = mapLayers.defendedTiles
    val constructionSiteFinder = new ConstructionSiteFinder(universe)

    def age = currentTick - created
  }
}

class HandleDefenses(universe: Universe) extends OrderlessAIModule[Mobile](universe) {

  private var backgroundOp = BWFuture.none[Seq[Group[Mobile]]]

  override def onTick_!(): Unit = {
    if (backgroundOp.result.isEmpty) {
      ifNth(Primes.prime43) {
        val allEnemies = mapLayers.defendedTiles.allBlocked.flatMap { tile =>
          unitGrid.enemy.onTile(tile)
        }.filterNot(_.isHarmlessNow)

        if (allEnemies.nonEmpty) {
          debug(s"Enemy detected! Calculating counterattack")
          val newBackgroundOp = GroupingHelper.groupThese(allEnemies, universe)
          backgroundOp = newBackgroundOp
        }
      }
    } else {
      backgroundOp.matchOnOptSelf(groups => {
        debug(s"Attackers grouped")
        val typedGroups = groups.map(GroupingHelper.typedGroup(universe, _))
        //prototype: just attack the biggest group with everything

        val biggest = typedGroups.maxBy(_.members.iterator.map(_.armorType.transportSize).sum)
        worldDominationPlan.initiateAttack(biggest.center, complete = false)

        resetBackgroundOp()
      }, {})
    }
  }

  private def resetBackgroundOp(): Unit = {
    backgroundOp = BWFuture.none
  }
}

class ProvideUpgrades(universe: Universe) extends OrderlessAIModule[Upgrader](universe) {
  self =>
  private val helper     = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val researched = collection.mutable.Map.empty[Upgrade, Int]

  override def onTick_!(): Unit = {
    val maxLimitEnabled = hasLimitDisabler
    val requested = {
      strategy.current.suggestUpgrades
      .filterNot(e => researched.getOrElse(e.upgrade, 0) == maxLimitEnabled.ifElse(e.maxLevel, 1))
      .filter(_.isActive)
    }

    requested.foreach { request =>
      val wantedUpgrade = request.upgrade
      val needs = race.techTree.upgraderFor(wantedUpgrade)
      val buildingPlannedOrExists = unitManager.existsOrPlanned(needs)
      if (buildingPlannedOrExists) {
        val buildMissing = !unitManager.plannedToBuild(needs) &&
                           ownUnits.allByClass(needs).size < bases.richBases.size

        val result = unitManager
                     .request(UnitJobRequest.upgraderFor(wantedUpgrade, self), buildMissing)
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
      } else {
        trace(s"Requesting ${
          needs.className
        } to be build in order for $wantedUpgrade to be researched")
        helper.requestBuilding(needs, takeCareOfDependencies = true)
      }
    }
  }

  private def hasLimitDisabler = universe.ownUnits.allByType[UpgradeLimitLifter].nonEmpty
}

class EnqueueFactories(universe: Universe)
  extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  override def onTick_!(): Unit = {
    evaluateCapacities.foreach { cap =>
      val existingByType = ownUnits.allByClass(cap.typeOfFactory).size +
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
      val copy = IdealProducerCount(elems.head.typeOfFactory, sum)(active = true)
      copy
    }
  }
}

case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory],
                                                maximumSustainable: Int)
                                               (active: => Boolean) {
  def isActive = active
}

case class IdealUnitRatio[T <: Mobile](unitType: Class[_ <: Mobile], amount: Int)
                                      (active: => Boolean) {
  def fixedAmount = amount max 1

  def isActive = active
}

class EnqueueArmy(universe: Universe)
  extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  override def onTick_!(): Unit = {
    val p = percentages
    val mostMissing = p.wanted.toVector.sortBy { case (t, idealRatio) =>
      val existingRatio = p.existing.getOrElse(t, 0.0)
      existingRatio / idealRatio
    }
    val needsSomething = {
      val (_, later) = mostMissing.partition
      { case (c, _) => universe.unitManager.allRequirementsFulfilled(c) }
      later.toSet
    }

    val canBuildNow = mostMissing.filterNot
    { case (c, _) => universe.unitManager.requirementsQueuedToBuild(c) }
    val highestPriority = canBuildNow
    val buildThese = highestPriority.view.takeWhile { e =>
      val rich = resources.couldAffordNow(e._1)
      def mineralsOverflow = resources.unlockedResources.moreMineralsThanGas &&
                             resources.unlockedResources.minerals > 400
      def gasOverflow = resources.unlockedResources.moreGasThanMinerals &&
                        resources.unlockedResources.gas > 400
      rich || mineralsOverflow || gasOverflow
    }
    buildThese.filterNot(needsSomething.contains).foreach { case (thisOne, _) =>
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
      val existing = ownUnits.allByType[Mobile].groupBy(_.getClass)
      summed.keySet.map { t =>
        t -> existing.get(t).map(_.size).getOrElse(0)
      }.toMap
    }

    val totalExisting = existingCounts.values.sum
    val percentagesExisting = existingCounts
                              .map { case (t, v) => t -> (if (totalExisting == 0) 0
                              else v.toDouble / totalExisting)
                              }
    Percentages(percentagesWanted, percentagesExisting)
  }

  case class Percentages(wanted: Map[Class[_ <: Mobile], Double],
                         existing: Map[Class[_ <: Mobile], Double])

}

object Strategy {

  trait LongTermStrategy extends HasUniverse {
    val timingHelpers = new TimingHelpers

    def buildAntiCloakNow: Boolean

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

        def isEarlyMid = isBetween(5, 9)

        def isBetween(from: Int, to: Int) = time.minutes >= from && time.minutes < to

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

    override def buildAntiCloakNow = {
      timingHelpers.phase.isSinceMid
    }

    override def suggestAddons: Seq[AddonToAdd] = {
      AddonToAdd(classOf[Comsat], requestNewBuildings = true)(
        unitManager.existsAndDone(classOf[Academy]) || timingHelpers.phase.isSinceEarlyMid) ::
      AddonToAdd(classOf[MachineShop], requestNewBuildings = false)(
        unitManager.existsAndDone(classOf[Factory])) ::
      AddonToAdd(classOf[ControlTower], requestNewBuildings = false)(
        unitManager.existsAndDone(classOf[Starport])) ::
      Nil
    }

    override def suggestNextExpansion = {
      val shouldExpand = expandNow
      if (shouldExpand) {
        val covered = bases.bases.map(_.resourceArea).toSet
        val dangerous = strategicMap.resources.filter { res =>
          universe.unitGrid.enemy.allInRange[Mobile](res.center, 15).nonEmpty
        }.toSet
        bases.mainBase.map(_.mainBuilding.tilePosition).flatMap { where =>
          val others = strategicMap.resources
                       .filterNot(covered)
                       .filterNot(dangerous)
                       .filter { where =>
                         universe.ownUnits.allByType[TransporterUnit].nonEmpty ||
                         mapLayers.rawWalkableMap
                         .areInSameWalkableArea(where.center,
                           bases.mainBase.get.mainBuilding.tilePosition)
                       }
          if (others.nonEmpty) {
            val target = others.maxBy(
              e => (e.mineralsAndGas, -e.patches.map(_.center.distanceSquaredTo(where))
                                       .getOrElse(999999)))
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
      val (poor, rich) = bases.myMineralFields
                         .partition(_.remainingPercentage < expansionThreshold)
      poor.size >= rich.size && rich.size <= 2
    }
    protected def expansionThreshold = 0.5
  }

  case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
    val maxLevel = upgrade.nativeType.fold(_.maxRepeats, _ => 1)

    def isActive = active
  }

  case class AddonToAdd(addon: Class[_ <: Addon], requestNewBuildings: Boolean)
                       (active: => Boolean) {
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
      ifNth(Primes.prime251) {
        best = available.maxBy(_.determineScore)
      }
    }
  }

  class IdleAround(override val universe: Universe) extends LongTermStrategy {
    override def buildAntiCloakNow = false

    override def determineScore = -1

    override def suggestProducers = Nil

    override def suggestUnits = Nil

    override def suggestUpgrades = Nil

    override def suggestNextExpansion = None
  }

  class TerranHeavyMetal(override val universe: Universe)
    extends LongTermStrategy with TerranDefaults {
    override def determineScore: Int = 50

    override def suggestProducers = {
      val myBases = bases.myMineralFields.count(_.remainingPercentage > 0.25)

      IdealProducerCount(classOf[Barracks], (myBases / 2) max 1)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], 2 + myBases)(timingHelpers.phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(
        timingHelpers.phase.isSincePostMid) ::
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
      IdealUnitRatio(classOf[Dropship], 1)(timingHelpers.phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 1)(timingHelpers.phase.isSinceLateMid) ::
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
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(
        timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      Nil
  }

  class TerranHeavyAir(override val universe: Universe)
    extends TerranAirSuperiority(universe) with TerranDefaults {
    override def suggestUnits: List[IdealUnitRatio[Nothing]] = {
      IdealUnitRatio(classOf[ScienceVessel], 3)(timingHelpers.phase.isAnyTime) ::
      IdealUnitRatio(classOf[Battlecruiser], 10)(timingHelpers.phase.isAnyTime) ::
      super.suggestUnits
    }

    override def determineScore: Int = {
      super.determineScore + bases.rich.ifElse(10, -10)
    }

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(timingHelpers.phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(timingHelpers.phase.isAnyTime) ::
      super.suggestUpgrades

  }

  class TerranAirSuperiority(override val universe: Universe)
    extends LongTermStrategy with TerranDefaults {

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
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicHeal)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(timingHelpers.phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(timingHelpers.phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(
        timingHelpers.phase.isSinceVeryLateMid) ::
      Nil
  }

  class TerranFootSoldiers(override val universe: Universe)
    extends LongTermStrategy with TerranDefaults {

    override def suggestUpgrades =
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(
        timingHelpers.phase.isSinceVeryEarlyMid) ::
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
