package pony
package brain
package jobs

import pony.brain.budget.ResourceApprovalSuccess
import pony.brain.requests.{CanAcceptSwitchAndHasFunding, CanAcceptUnitSwitch, UnitJobRequest}
import pony.geometry.{Area, MapTilePosition, Size}
import pony.pathing.PathFinder
import pony.terrain.ResourceArea
import pony.units.{Building, Bunker, MainBuilding, WorkerUnit}

import pony.brain.requests.UnitRequest.CherryPickers

import scala.reflect.ClassTag

class ConstructBuilding[W <: WorkerUnit: ClassTag, B <: Building](
    worker: W,
    buildingType: Class[? <: B],
    employer: Employer[W],
    val buildWhere: MapTilePosition,
    funding: ResourceApprovalSuccess,
    val belongsTo: Option[ResourceArea] = None,
    sharedTravelProgress: Option[ConstructionTravelProgress] = None
) extends UnitWithJob[W](employer, worker, Priority.ConstructBuilding)
    with JobHasFunding[W]
    with CreatesUnit[W]
    with CanAcceptUnitSwitch[W]
    with FerrySupport[W]
    with CanAcceptSwitchAndHasFunding[W]
    with PathfindingSupport[W]
    with IssueOrderNTimes[W] {
  self =>

  private lazy val travelProgress = sharedTravelProgress.getOrElse {
    val target =
      if (
        strategy.current.runsTerranCampaign &&
        (isMainBuilding || buildingType == classOf[Bunker])
      ) Some(buildWhere)
      else None
    new ConstructionTravelProgress(currentTick, unit.currentTile, target)
  }
  private var arrivalCommandsReset = false

  assert(
    universe.mapLayers.rawWalkableMap.insideBounds(buildWhere),
    s"Target building spot is outside of map: $buildWhere, check $self"
  )

  val area = {
    val unitType = buildingType.toUnitType
    val size     = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    Area(buildWhere, size)
  }

  /**
    * Close enough to the footprint to build. Measured from the footprint, not its upper left tile: a refinery's corner
    * lies inside the geyser, and in game 3 vs Zerg on 050c38f an SCV beside the geyser's east side stood 4.5 tiles from
    * it, was ordered to walk on, could not, and twice gave up, which cost the first refinery three minutes.
    */
  private def atSite(tile: MapTilePosition) = area.distanceTo(tile) < 3
  private val isMainBuilding                = classOf[MainBuilding] >= buildingType

  listen_!((failed: Boolean) => {
    mapLayers.unblockBuilding_!(area)
  })

  assert(
    resources.detailedLocks.exists(e => e.whatFor == buildingType && e.reqs.sum == funding.sum),
    s"Something is wrong, check $this, it is supposed to have ${
        funding.sum
      } funding, but the resource manager only has\n ${
        resources.detailedLocks.mkString("\n")
      }\nlocked"
  )
  private var startedMovingToSite       = false
  private var startedActualConstruction = false
  private var finishedConstruction      = false
  private var resourcesUnlocked         = false
  private var constructs                = Option.empty[Building]

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (unit.gotUnloaded) resetTimer_!()
    if (!arrivalCommandsReset && atSite(unit.currentTile)) {
      resetIssuedOrders_!()
      resetTimer_!()
      arrivalCommandsReset = true
    }
    if (startedActualConstruction && !resourcesUnlocked && constructs.isDefined) {
      trace(s"Construction job $this no longer needs to lock resources")
      unlockManually_!()
      resourcesUnlocked = true
      mapLayers.unblockBuilding_!(area)
    }
  }

  override def stillWantsOptimization = canSwitchNow &&
    buildWhere.distanceToIsMore(unit.currentTile, 3) &&
    stillLocksResources &&
    !failedOrObsolete &&
    unit.onGround

  override def canSwitchNow = {
    !startedActualConstruction
  }

  override def getOrder: Seq[UnitOrder] = {
    Orders.ConstructBuilding(worker, buildingType, buildWhere).toSeq
  }

  override def higherPriorityOrder: Seq[UnitOrder] = {
    val prior = super.higherPriorityOrder
    if (prior.nonEmpty) {
      prior
    } else if (!startedActualConstruction && worker.onGround && !atSite(worker.currentTile)) {
      // Far build sites are refused natively; walk there first.
      Seq(Orders.MoveToTile(worker, detourStep.getOrElse(buildWhere)).lockingFor_!(24))
    } else {
      Nil
    }
  }

  private var stillAt    = unit.currentTile
  private var stillSince = currentTick
  private var detour     = Option.empty[MapTilePosition]

  /**
    * A short step along our own path when the game will not move the builder. StarCraft routes through the middle of
    * its pathing regions and ignores our buildings: in game 3 vs Terran on 9497320 on ICCup Lost_Temple the SCV that
    * built the first wall depot stood between it and the gate barracks, each order to walk to the second depot's site
    * ended where it stood, and the wall stayed open all game while the same SCV failed seven times.
    */
  private def detourStep: Option[MapTilePosition] = {
    val here = worker.currentTile
    if (here != stillAt) {
      stillAt = here
      stillSince = currentTick
      if (detour.exists(_.distanceToIsLess(here, 2))) detour = None
    }
    if (detour.isEmpty && currentTick - stillSince >= ConstructBuilding.StillFrames) {
      stillSince = currentTick
      detour = PathFinder.on(mapLayers.freeWalkableIgnoringMobiles, isOnGround = true)
        .findSimplePathNow(here, area.centerTile)
        .flatMap(_.waypoints.takeWhile(_.distanceToIsLess(here, ConstructBuilding.DetourTiles + 1)).lastOption)
        .filter(_ != here)
      NativeMatchEvidence.trace(
        "construction-detour",
        s"type=${typeOfBuilding.className} where=$buildWhere worker=${worker.nativeUnitId} at=$here step=$detour"
      )
    }
    detour
  }

  override def shortDebugString: String = s"Build ${buildingType.className}"

  override def proofForFunding = funding

  override def couldSwitchInTheFuture = !startedActualConstruction

  private var failureTraced = false

  override def jobHasFailedWithoutDeath: Boolean = {
    // a builder waiting for its ferry makes no progress, and its site is no less reachable for that
    if (unit.onGround && !waitsForFerry) {
      val byState = {
        travelProgress.failed(
          currentTick,
          unit.currentTile,
          atSite(unit.currentTile),
          worker.isConstructingBuilding,
          times + 10
        ) &&
        !isFinished
      }
      def expensiveCheck = {
        def targetBlockedByBuilding = {
          !mapLayers.blockedByBuildingTiles.free(area) && constructs.isEmpty
        }

        mapNth(Primes.prime37, false)(targetBlockedByBuilding)
      }

      val blocked = expensiveCheck
      val fail    = byState || blocked
      if (fail && !failureTraced) {
        failureTraced = true
        // stalled short of the site: the next spot for this kind of building must lie elsewhere
        if (byState && !atSite(unit.currentTile))
          unitManager.unreachableSites = unitManager.unreachableSites :+ area
        NativeMatchEvidence.trace(
          "construction-failed",
          s"type=${typeOfBuilding.className} where=$buildWhere worker=${worker.nativeUnitId} at=${unit.currentTile} " +
            s"stalled=$byState blocked=$blocked order=${worker.nativeUnit.getOrder}"
        )
      }
      warn(
        s"Construction of ${typeOfBuilding.className} failed, worker $worker didn't manange",
        fail
      )

      fail
    } else {
      false
    }
  }

  def typeOfBuilding = buildingType

  override def isFinished = {
    if (!startedMovingToSite) {
      startedMovingToSite = worker.isInConstructionProcess
      trace(s"Worker $worker started to move to construction site", startedMovingToSite)
    } else if (!startedActualConstruction) {
      startedActualConstruction = worker.isConstructingBuilding
      if (startedActualConstruction) {
        constructs = employer.ownUnits.buildingAt(buildWhere)
        // i saw this going wrong once
        if (constructs.isEmpty) {
          fail_!()
        }
      }
      trace(s"Worker $worker started to build $buildingType", startedActualConstruction)
    } else if (!finishedConstruction) {
      assert(constructs.isDefined)
      finishedConstruction = !worker.isInConstructionProcess
      trace(s"Worker $worker finished to build $buildingType", finishedConstruction)
    }
    ageSinceLastReset > times + 10 && startedMovingToSite && finishedConstruction
  }

  override def times = 50

  def building = constructs

  /**
    * The builder is weighed once, when the job is planned (ProvideNewBuildings), by straight distance. A search for the
    * worker with the shortest path used to run beside it, repeated every 43 frames while the builder walked. It threw for
    * any tile without a straight line to one of its paths' waypoints, so it never gave a result: in the ten games on
    * ICCup Lost_Temple at 74b88f4 it failed 15 to 86 times per game, and no builder was ever swapped by it. Every tested
    * game ran without it, so it is gone, along with its load on the bot's two worker threads.
    */
  override def asRequest: UnitJobRequest[W] = {
    val picker = CherryPickers.cherryPickWorkerByDistance[W](area.centerTile)()
    UnitJobRequest.constructor[W](employer).withRequest(_.withCherryPicker_!(picker))
  }

  override def copyOfJobForNewUnit(replacement: W) = {
    assert(!failedOrObsolete)
    stopManagingResource_!()
    markObsolete_!()
    new ConstructBuilding(
      replacement,
      buildingType,
      employer,
      buildWhere,
      funding,
      belongsTo,
      Some(travelProgress)
    )
  }

  override protected def pathTargetPosition = {
    buildWhere.toSome
  }

  override protected def ferryDropTarget = area.centerTile.toSome
}

object ConstructBuilding {

  /** Frames a builder short of its site stands on one tile before it steps along our own path instead. */
  val StillFrames = 48

  /** How far one such step goes, in tiles: short, so the game never has to route it between its pathing regions. */
  val DetourTiles = 3
}
