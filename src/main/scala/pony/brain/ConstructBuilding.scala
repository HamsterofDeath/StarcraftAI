package pony
package brain

import pony.brain.UnitRequest.CherryPickers
import pony.brain.modules.Strategy

import scala.collection.mutable
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
        strategy.current.isInstanceOf[Strategy.SimpleTerran] &&
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
  private val alternativeWorkers = {
    class Input {
      val target     = buildWhere
      val pathfinder = pathfinders.groundSafe
      val candidates = unitManager.ownUnits.allByType[W].map { w =>
        w.currentTile
      }
    }

    FutureIterator.feed(new Input).produceAsyncLater { in =>
      val paths = in.candidates.flatMap { where =>
        in.pathfinder.findSimplePathNow(where, in.target, tryFixPath = true)
      }
      val data = new ClosestPaths(paths)
      in.candidates.foreach { pos =>
        mapLayers.rawWalkableMap.spiralAround(pos, 5).foreach { precalcThis =>
          data.closestPathFrom(precalcThis)
        }
      }
      data
    }.named("Find worker path")
  }
  private val isMainBuilding = classOf[MainBuilding] >= buildingType

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
    if (!arrivalCommandsReset && unit.currentTile.distanceToIsLess(buildWhere, 4)) {
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

    if (stillWantsOptimization) {
      // regularly try to optimize
      ifNth(Primes.prime43) {
        alternativeWorkers.prepareNextIfDone()
      }
      if (alternativeWorkers.hasResult) {
        unitManager.tryFindBetterEmployeeFor(this)
      }
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
    } else if (!startedActualConstruction && worker.onGround && buildWhere.distanceToIsMore(worker.currentTile, 4)) {
      // Far build sites are refused natively; walk there first.
      Seq(Orders.MoveToTile(worker, buildWhere).lockingFor_!(24))
    } else {
      Nil
    }
  }

  override def shortDebugString: String = s"Build ${buildingType.className}"

  override def proofForFunding = funding

  override def couldSwitchInTheFuture = !startedActualConstruction

  override def jobHasFailedWithoutDeath: Boolean = {
    if (unit.onGround) {
      val byState = {
        travelProgress.failed(
          currentTick,
          unit.currentTile,
          unit.currentTile.distanceToIsLess(buildWhere, 4),
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

      val fail = byState || expensiveCheck
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

  override def asRequest: UnitJobRequest[W] = {
    val picker = alternativeWorkers.mostRecent match {
      case Some(data) =>
        CherryPickers.cherryPickWorkerByDistance[W](area.centerTile) { from =>
          data.closestPathFrom(from)
        }
      case None =>
        CherryPickers.cherryPickWorkerByDistance[W](area.centerTile)()
    }

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

  private class ClosestPaths(source: Iterable[Path]) {
    private val cache = mutable.HashMap.empty[MapTilePosition, Double]

    def closestPathFrom(here: MapTilePosition) = {
      cache.getOrElseUpdate(
        here, {
          val candidates = source.map { path =>
            if (path.waypoints.nonEmpty) {
              path -> path.distanceToFinalTargetViaPath(here).getOr(s"Should never happen")
            } else {
              path -> 0.0
            }
          }
          candidates.minBy(_._2)._2
        }
      )
    }
  }
}
