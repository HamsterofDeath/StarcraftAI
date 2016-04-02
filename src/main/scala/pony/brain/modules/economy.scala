package pony
package brain
package modules

import bwapi.Color
import pony.brain.UnitRequest.CherryPickers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProvideNewBuildings(universe: Universe)
  extends AIModule[WorkerUnit](universe) with BackgroundComputation[WorkerUnit] {
  self =>

  override type ComputationInput = Data

  override def evaluateNextOrders(in: ComputationInput) = {
    val helper = in.helper
    val job = {
      def newJob(where: MapTilePosition) =
        new ConstructBuilding(in.worker, in.buildingType, self, where,
          in.jobRequest.proofForFunding.assumeSuccessful,
          in.jobRequest.belongsTo)

      val customPosition = in.jobRequest.customPosition.predefined
                           .orElse(in.jobRequest.customPosition.evaluateCostly)
                           .orElse(
                             helper.findSpotFor(in.mainBuildingwhere, in.buildingType))

      customPosition
      .foreach(e => assert(mapLayers.rawWalkableMap.insideBounds(e), s"$e was not inside map :("))
      customPosition.map(where => () => newJob(where))
    }

    job match {
      case None =>
        error(s"Computation returned no result: $in")
        BackgroundComputationResult.nothing[WorkerUnit](() => {
          if (in.jobRequest.stillLocksResources) {
            in.jobRequest.forceUnlockOnDispose_!()
            in.jobRequest.dispose()
          } else {
            warn(s"Why is the same request processed again? -> ${in.jobRequest}")
          }
        })
      case Some(newJobFactory) =>
        BackgroundComputationResult
        .result[WorkerUnit, ConstructBuilding[WorkerUnit, _ <: Building]](
          myJobs = newJobFactory.toSeq,
          checkValidityNow = () => false) { jobs =>
          val job = jobs.headAssert
          info(s"Planning to build ${in.buildingType.className} at ${job.buildWhere} by ${
            in.worker
          }")
          in.jobRequest.clearableInNextTick_!()
          mapLayers.blockBuilding_!(job.area)
          //maybe there is a better worker for this than the one that was initially chosen
          unitManager.tryFindBetterEmployeeFor(job)
        }
    }
  }

  override def calculationInput = {
    // we do them one by one, it's simpler. in the current tick, the same missing buildings will
    // still be
    // requested, so the queue will be refilled
    val buildingRelated = unitManager.failedToProvideByType[Building]
    val constructionRequests = buildingRelated.iterator.collect {
      case buildIt: BuildUnitRequest[Building]
        if buildIt.proofForFunding.isSuccess &&
           !buildIt.isAddon &&
           universe.resources.hasStillLocked(buildIt.funding) =>
        buildIt
    }

    val anyOfThese = constructionRequests.find { candidate =>
      val passThrough = !candidate.isUpgrader
      def isUpgradeEnablerOrRich = {
        val count = ownUnits.allByClass(candidate.typeOfRequestedUnit).size
        val isFirst = count == 0
        isFirst || bases.rich && bases.bases.size > count
      }
      passThrough || isUpgradeEnablerOrRich
    }

    anyOfThese.flatMap { req =>
      val request = UnitJobRequest.constructor(self)
      unitManager.request(request) match {
        case success: ExactlyOneSuccess[WorkerUnit] =>
          req.customPosition.init_!()
          val randomWorker = success.onlyOne
          new Data(randomWorker, req.typeOfRequestedUnit,
            unitManager.bases.mainBase.getOr("All your base are belong to us"),
            new ConstructionSiteFinder(universe), req).toSome
        case _ => None
      }
    }
  }

  case class Data(worker: WorkerUnit, buildingType: Class[_ <: Building], base: Base,
                  helper: ConstructionSiteFinder,
                  jobRequest: BuildUnitRequest[Building]) {
    val mainBuildingwhere = base.mainBuilding.tilePosition
  }

}

class ProvideExpansions(universe: Universe)
  extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private var plannedExpansionPoint = Option.empty[ResourceArea]

  def forceExpand(patch: MineralPatchGroup) = {
    plannedExpansionPoint = {
      val all = world.resourceAnalyzer.resourceAreas
      all.find(_.isPatchId(patch.patchId))
    }
  }

  override def renderDebug(renderer: Renderer) = {
    plannedExpansionPoint.foreach { res =>
      renderer.in_!(Color.White).drawTextAtTile("   Ex", res.center)
    }
  }

  override def onTick_!(): Unit = {
    ifNth(Primes.prime241) {
      plannedExpansionPoint = plannedExpansionPoint.filter { where =>
        universe.mapLayers.slightlyDangerousAsBlocked.free(where.center) &&
        universe.unitGrid.enemy.allInRange(where.center, 12).isEmpty &&
        !bases.isCovered(where)
      }.orElse(strategy.current.suggestNextExpansion)
      info(s"AI wants to expand to ${plannedExpansionPoint.get}", plannedExpansionPoint.isDefined)
    }

    plannedExpansionPoint.filter { _ =>
      // only build one at a time
      unitManager.plannedToBuildByType[MainBuilding] == 0 &&
      unitManager.constructionsInProgress[MainBuilding].isEmpty
    }.foreach { resources =>
      val plannedMainBuildings = unitManager.constructionsInProgress(race.resourceDepositClass)
      plannedMainBuildings.find(_.belongsTo.contains(resources)) match {
        case Some(planned) =>
          if (planned.building.isDefined) {
            trace(s"New expansion exists, resetting expansion plans")
            plannedExpansionPoint = None
          }
        case None =>
          if (!unitManager.plannedToBuild(race.resourceDepositClass)) {
            val buildingSpot = AlternativeBuildingSpot
                               .fromExpensive(
                                 new ConstructionSiteFinder(universe).forResourceArea(resources))(
                                 _.find)
            requestBuilding(race.resourceDepositClass, takeCareOfDependencies = false,
              saveMoneyIfPoor = true,
              buildingSpot, belongsTo = plannedExpansionPoint, priority = Priority.Expand)
          }
      }
    }
  }
}

class ProvideNewSupply(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) {
  private val supplyEmployer = new Employer[SupplyProvider](universe)

  override def onTick_!() = {

    val cur = plannedSupplies
    val needsMore = cur.supplyUsagePercent >= 0.8 && cur.total < 400

    trace(s"Need more supply: $cur ($plannedSupplies planned)", needsMore)
    if (needsMore) {
      // can't use helper because overlords are not buildings :|
      val race = universe.myRace
      val result = resources.request(
        ResourceRequests.forUnit(race, classOf[SupplyProvider], Priority.Supply), this)
      result.ifSuccess { suc =>
        trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
        val ofType = UnitJobRequest
                     .newOfType(universe, supplyEmployer, classOf[SupplyProvider], suc,
                       priority = Priority.Supply)

        // this will always be unfulfilled
        val result = unitManager.request(ofType)
        assert(!result.success, s"Impossible success: $result")
      }
    }
  }

  private def plannedSupplies = {
    val real = resources.supplies
    val planned = unitManager.plannedSupplyAdditions
    real.copy(total = real.total + planned)
  }
}

class ProvideSpareSCVs(universe: Universe) extends OrderlessAIModule[CommandCenter](universe) {

  private val emp = new Employer[WorkerUnit](universe)

  override def onTick_!() = {
    super.onTick_!()
    ifNth(Primes.prime37) {
      // only for terran!
      val ok = unitManager.allJobsByUnitType[SCV].exists(_.isIdle)
      if (!ok) {
        unitManager.request(UnitJobRequest.idleOfType(emp, classOf[WorkerUnit], 1))
      }
    }
  }
}

class ProvideNewUnits(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  self =>

  override def onTick_!(): Unit = {
    unitManager.failedToProvideFlat.distinct.flatMap { req =>
      trace(s"Trying to satisfy $req somehow")
      val wantedType = req.typeOfRequestedUnit
      if (classOf[Mobile].isAssignableFrom(wantedType)) {
        val typeFixed = wantedType.asInstanceOf[Class[Mobile]]
        val wantedAmount = req.amount
        1 to wantedAmount flatMap { _ =>
          val builderOf = UnitJobRequest.builderOf(typeFixed, self)
          unitManager.request(builderOf) match {
            case any: ExactlyOneSuccess[UnitFactory] =>
              val producer = any
              unitManager.jobOf(producer.onlyOne) match {
                case t: CreatesUnit[_] =>
                  req.clearableInNextTick_!()
                  Nil
                case _ =>
                  val res = req match {
                    case hf: HasFunding if resources.hasStillLocked(hf.proofForFunding) => hf
                                                                                           .proofForFunding
                    case _ => resources
                              .request(
                                ResourceRequests.forUnit(universe.myRace, typeFixed, req.priority),
                                self)
                  }
                  res match {
                    case suc: ResourceApprovalSuccess =>
                      // job will take care of resource disposal
                      req.keepResourcesLocked_!()
                      req.clearableInNextTick_!()
                      val order = new TrainUnit(any.onlyOne, typeFixed, self, suc)
                      assignJob_!(order)
                      Nil
                    case _ =>
                      req.clearableInNextTick_!()
                      Nil
                  }
              }
            case _ =>
              req.clearableInNextTick_!()
              Nil
          }
        }
      } else {
        Nil
      }
    }
  }
}

class DefaultBehaviours(universe: Universe) extends OrderlessAIModule[WrapsUnit](universe) {
  self =>
  private val rules  = Terran.allBehaviours(universe)
  private val ignore = mutable.HashSet.empty[WrapsUnit]

  override def renderDebug(renderer: Renderer): Unit = {
    rules.foreach(_.renderDebug_!(renderer))
  }

  override def onTick_!(): Unit = {
    rules.foreach(_.onTick_!())
    // fetch all idles and assign "always on" background tasks to them
    val hireUs = unitManager.allIdles.filterNot(e => ignore(e.unit)).flatMap { free =>
      val unit = free.unit
      rules.filter(_.canControl(unit)).foreach(_.add_!(unit, Objective.initial))
      val behaviours = rules.filter(_.controls(unit)).flatMap(_.behaviourOf(unit))
      if (behaviours.nonEmpty) {
        Some(new BusyDoingSomething(self, behaviours, Objective.initial))
      } else {
        ignore += unit
        None
      }
    }
    info(s"Attaching default behaviour to new ${hireUs.size} units", hireUs.nonEmpty)
    hireUs.foreach {unitManager.assignJob_!}
  }
}

class ManageMiningAtBases(universe: Universe) extends OrderlessAIModule(universe) {

  private val gatheringJobs = ArrayBuffer.empty[ManageMiningAtPatchGroup]

  override def onTick_!(): Unit = {
    createJobsForBases()
    gatheringJobs.foreach(_.onTick_!())
  }

  private def createJobsForBases(): Unit = {
    val add = universe.bases
              .bases
              .filterNot(e => gatheringJobs.exists(_.covers(e)))
              .filterNot(_.mainBuilding.isBeingCreated)
              .flatMap { base =>
                base.myMineralGroup.map { minerals =>
                  new ManageMiningAtPatchGroup(base, minerals)
                }
              }
    info(
      s"""
         |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin, add.nonEmpty)
    gatheringJobs ++= add
  }

  class ManageMiningAtPatchGroup(base: Base, minerals: MineralPatchGroup)
    extends Employer[WorkerUnit](universe) {
    emp =>

    override def onTick_!(): Unit = {
      super.onTick_!()
      minerals.tick()
      Micro.MiningOrganization.onTick()
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        val result = universe.unitManager
                     .request(UnitJobRequest.idleOfType(emp, classOf[WorkerUnit], missing))
        val jobs = result.units.flatMap { worker =>
          Micro.MiningOrganization.findBestPatch(worker).map { patch =>
            info(s"Added $worker to mining team of $patch")
            val job = new Micro.MineMineralsAtPatch(worker, patch)
            patch.lockToPatch_!(job)
            job
          }
        }
        jobs.foreach(assignJob_!)
      }
    }

    private def idealNumberOfWorkers = Micro.MiningOrganization.idealNumberOfWorkers

    def covers(aBase: Base) = base == aBase

    override def toString = s"Gathering $minerals at $base"

    object Micro {

      class MinedPatch(val patch: MineralPatch) {

        private val miningTeam            = ArrayBuffer.empty[MineMineralsAtPatch]
        private val workerCountByDistance = LazyVal.from {
          val distance = math.round(patch.area.distanceTo(base.mainBuilding.area)).toInt
          distance match {
            case 0 => 1
            case 1 | 2 | 3 | 4 => 2
            case 5 | 6 | 7 => 3
            case x => (x / 2.5).toInt
          }
        }

        override def toString: String = s"(Mined) $patch"

        def hasOpenSpot: Boolean = miningTeam.size < estimateRequiredWorkers

        def openSpotCount = estimateRequiredWorkers - miningTeam.size

        def estimateRequiredWorkers = {
          if (patch.remainingMinerals > 0) workerCountByDistance.get else 0
        }

        def lockToPatch_!(job: MineMineralsAtPatch): Unit = {
          info(s"Added ${job.unit} to mining team of $patch")
          miningTeam += job
        }

        def isInTeam(worker: WorkerUnit) = miningTeam.exists(_.unit == worker)

        def removeFromPatch_!(worker: WorkerUnit): Unit = {
          info(s"Removing $worker from mining team of $patch")
          val found = miningTeam.find(_.unit == worker)
          assert(found.isDefined, s"Did not find $worker in $miningTeam")
          found.foreach {miningTeam -= _}
        }
      }

      class MineMineralsAtPatch(myWorker: WorkerUnit, miningTarget: MinedPatch)
        extends UnitWithJob(emp, myWorker, Priority.Default)
                with GatherMineralsAtSinglePatch
                with CanAcceptUnitSwitch[WorkerUnit]
                with Interruptable[WorkerUnit]
                with FerrySupport[WorkerUnit]
                with PathfindingSupport[WorkerUnit] {

        listen_!(failed => {
          if (miningTarget.isInTeam(myWorker)) {
            miningTarget.removeFromPatch_!(myWorker)
          }
        })

        import States._

        private val nearestReachableBase = oncePer(Primes.prime71) {
          bases.bases.filter { b =>
            worker.currentArea.contains(b.mainBuilding.areaOnMap)
          }.minByOpt { base =>
            base.mainBuilding.area.distanceTo(worker.currentTile)
          }
        }
        private var state: State         = Idle

        override def copyOfJobForNewUnit(replacement: WorkerUnit) = new
            MineMineralsAtPatch(replacement, miningTarget)

        override def asRequest = {
          val picker = {
            CherryPickers.cherryPickWorkerByDistance[WorkerUnit](miningTarget.patch.centerTile)()
          }
          UnitJobRequest.idleOfType(emp, myWorker.getClass)
          .withRequest(_.withCherryPicker_!(picker))
        }

        override def couldSwitchInTheFuture = miningTarget.patch.hasRemainingMinerals

        override def canSwitchNow = !worker.isWaitingForMinerals &&
                                    !worker.isCarryingMinerals &&
                                    !worker.isInMiningProcess

        override def onStealUnit(): Unit = {
          super.onStealUnit()
          miningTarget.removeFromPatch_!(myWorker)
        }

        override def requiredWorkers: Int = miningTarget.estimateRequiredWorkers

        override def shortDebugString: String = state match {
          case States.Idle => s"Idle/${unit.nativeUnit.getOrder}"
          case States.ApproachingMinerals => "Locked"
          case States.Mining => "Mining"
          case States.ReturningMinerals => "Delivering"
          case States.ReturningMineralsAfterInterruption => "Delivering (really)"
        }

        override def ordersForTick: Seq[UnitOrder] = {
          def sendWorkerToPatch = ApproachingMinerals -> Orders.Gather(myWorker, miningTarget.patch)
          def returnDelivery = Orders.ReturnMinerals(myWorker, base.mainBuilding)
          if (myWorker.isGuarding) {
            state = Idle
          }
          def noop = Orders.NoUpdate(myWorker)
          // TODO use speed cheat
          val (newState, order) = state match {
            case Idle if myWorker.isCarryingMinerals =>
              ReturningMineralsAfterInterruption -> returnDelivery
            // start going to your patch using wall hack
            case Idle if !myWorker.isCarryingMinerals =>
              sendWorkerToPatch
            // keep going while mining has not been started

            case ApproachingMinerals if miningTarget.patch.isBeingMined =>
              // repeat the order to prevent the worker from moving away
              sendWorkerToPatch

            case ApproachingMinerals if myWorker.isWaitingForMinerals ||
                                        myWorker.isInMiningProcess =>
              // let the poor worker alone now
              Mining -> noop

            case ApproachingMinerals =>
              ApproachingMinerals -> noop

            case Mining if myWorker.isInMiningProcess =>
              // let it work
              Mining -> noop
            case Mining if myWorker.isCarryingMinerals =>
              // the worker is done mining
              ReturningMinerals -> returnDelivery

            case ReturningMineralsAfterInterruption if myWorker.isCarryingMinerals &&
                                                       !myWorker.isMoving =>
              noCommandsForTicks_!(10)
              ReturningMineralsAfterInterruption -> returnDelivery
            case ReturningMineralsAfterInterruption if myWorker.isCarryingMinerals =>
              noCommandsForTicks_!(10)
              ReturningMinerals -> returnDelivery

            case ReturningMinerals if myWorker.isCarryingMinerals =>
              ReturningMinerals -> noop
            case ReturningMinerals if !myWorker.isCarryingMinerals =>
              // switch back to mining mode
              sendWorkerToPatch

            case _ =>
              Idle -> noop
          }
          state = newState
          order.toList.filterNot(_.isNoop)
        }

        override def isFinished = !miningTarget.patch.isInGame

        override def failedOrObsolete: Boolean = super.failedOrObsolete || base.mainBuilding.isDead

        override protected def pathTargetPosition = {
          if (worker.isCarryingMinerals) {
            nearestReachableBase.map(_.mainBuilding.centerTile)
          } else {
            miningTarget.patch.centerTile.toSome
          }
        }

        override def worker = myWorker

        override protected def ferryDropTarget = {
          targetPatch.tilePosition.middleBetween(base.mainBuilding.tilePosition).toSome
        }

        override def targetPatch = miningTarget.patch
      }

      object MiningOrganization {
        private val assignments = mutable.HashMap.empty[MineralPatch, MinedPatch]

        minerals.patches.foreach { mp =>
          assignments.put(mp, new MinedPatch(mp))
        }

        def onTick(): Unit = {
          val remove = assignments.keysIterator.filterNot(_.isInGame).toSet
          assignments --= remove
        }

        def findBestPatch(worker: WorkerUnit) = {
          val maxFreeSlots = assignments.iterator.map(_._2.openSpotCount).max
          val notFull = assignments.filter(_._2.openSpotCount == maxFreeSlots)
          if (notFull.nonEmpty) {
            val (_, patch) = notFull.minBy { case (mins, _) =>
              mins.area.closestDirectConnection(worker.blockedArea).length
            }
            Some(patch)
          } else
            None
        }

        def idealNumberOfWorkers = assignments.valuesIterator.map(_.estimateRequiredWorkers).sum

      }

      object States {

        sealed trait State

        case object Idle extends State

        case object ApproachingMinerals extends State

        case object Mining extends State

        case object ReturningMinerals extends State

        case object ReturningMineralsAfterInterruption extends State

      }

    }

  }

}

class ManageMiningAtGeysirs(universe: Universe)
  extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private val gatheringJobs = ArrayBuffer.empty[ManageMiningAtGeysir]

  override def onTick_!(): Unit = {
    val unattended = unitManager.bases.bases.filter(base => !gatheringJobs.exists(_.covers(base)))
    unattended.foreach { base =>
      base.myGeysirs.map { geysir =>
        new ManageMiningAtGeysir(base, geysir)
      }.foreach {gatheringJobs += _}
    }

    gatheringJobs.retain(_.keep).foreach(_.onTick_!())
  }

  class ManageMiningAtGeysir(base: Base, geysir: Geysir) extends Employer[WorkerUnit](universe) {
    self =>
    private val idealWorkerCount            = 3 +
                                              (base.mainBuilding.area.distanceTo(geysir.area) / 3)
                                              .toInt
    private val workerCountBeforeWantingGas = universe.mapLayers
                                              .isOnIsland(base.mainBuilding.tilePosition)
                                              .ifElse(8, 14)
    private var refinery                    = Option.empty[Refinery]

    override def toString = s"GetGas@${geysir.tilePosition}"

    def keep = geysir.isInGame

    override def onTick_!(): Unit = {
      super.onTick_!()
      refinery = refinery.filter(_.isInGame)
      refinery match {
        case None =>
          if (ownUnits.allByType[WorkerUnit].size >= workerCountBeforeWantingGas) {
            def requestExists = unitManager.requestedConstructions[Refinery]
                                .exists(_.customPosition.predefined.contains(geysir.tilePosition))
            def jobExists = unitManager.constructionsInProgress[Refinery]
                            .exists(_.buildWhere == geysir.tilePosition)
            def findAndRememberRefinery() = refinery.orElse(ownUnits.allByType[Refinery]
                                                            .find(
                                                              _.tilePosition == geysir.tilePosition)
                                                            .filterNot(_.isBeingCreated)
                                                            .flatMap { refinery =>
                                                              self.refinery = Some(refinery)
                                                              self.refinery
                                                            })

            if (!requestExists && !jobExists && findAndRememberRefinery().isEmpty) {
              val where = AlternativeBuildingSpot.fromPreset(geysir.tilePosition)
              requestBuilding(classOf[Refinery], customBuildingPosition = where)
            }
          }
        case Some(ref) =>
          val missing = idealWorkerCount - teamSize
          if (missing > 0) {
            val ofType = UnitJobRequest
                         .idleOfType(self, classOf[WorkerUnit], missing, Priority.CollectGas)
                         .withOnlyAccepting(_.isCarryingNothing)
            val result = unitManager.request(ofType)
            result.units.foreach { freeWorker =>
              assignJob_!(new MineGasAtGeysir(freeWorker, geysir))
            }
          }
      }
    }

    def covers(base: Base) = this.base.mainBuilding == base.mainBuilding

    class MineGasAtGeysir(worker: WorkerUnit, targetGeysir: Geysir)
      extends UnitWithJob[WorkerUnit](self, worker, Priority.ConstructBuilding)
              with Interruptable[WorkerUnit]
              with CanAcceptUnitSwitch[WorkerUnit]
              with FerrySupport[WorkerUnit]
              with PathfindingSupport[WorkerUnit] {
      private val freeNearGeysir       = {
        mapLayers.rawWalkableMap.nearestFreeBlock(geysir.tilePosition, 1)
      }
      private val nearestReachableBase = oncePer(Primes.prime71) {
        bases.bases.filter { b =>
          worker.currentArea.contains(b.mainBuilding.areaOnMap)
        }.minByOpt { base =>
          base.mainBuilding.area.distanceTo(worker.currentTile)
        }
      }
      private var state: State         = Idle

      override def copyOfJobForNewUnit(replacement: WorkerUnit) = new
          MineGasAtGeysir(replacement, targetGeysir)

      override def asRequest = {
        val picker = {
          CherryPickers.cherryPickWorkerByDistance[WorkerUnit](targetGeysir.centerTile)()
        }
        UnitJobRequest.idleOfType(employer, worker.getClass)
        .withRequest(_.withCherryPicker_!(picker))
      }

      override def couldSwitchInTheFuture = geysir.nonEmpty

      override def canSwitchNow = !worker.isCarryingGas && !worker.isGatheringGas

      override def jobHasFailedWithoutDeath = {
        super.jobHasFailedWithoutDeath || refinery.isEmpty || refinery.exists(_.isDead)
      }

      override def shortDebugString: String = state.getClass.className

      override def isFinished = false

      override def ordersForTick: Seq[UnitOrder] = {
        val (newState, order) = state match {
          case Idle if refinery.isDefined =>
            Gathering -> Orders.Gather(worker, refinery.get)
          case Gathering if worker.isGatheringGas || worker.isMagic =>
            Gathering -> Orders.NoUpdate(worker)
          case _ =>
            Idle -> Orders.NoUpdate(worker)
        }
        state = newState
        order.toSeq
      }

      override protected def pathTargetPosition = {
        if (worker.isCarryingGas) {
          nearestReachableBase.map(_.mainBuilding.centerTile)
        } else {
          geysir.centerTile.toSome
        }
      }

      override protected def ferryDropTarget = {
        freeNearGeysir.forNone {
          warn(s"No free tile around $geysir")
        }
        freeNearGeysir
      }

      trait State

      case object Idle extends State

      case object Gathering extends State

    }

  }

}

// need this for instanceof checks
trait GatherMineralsAtSinglePatch extends UnitWithJob[WorkerUnit] {
  def worker: WorkerUnit
  def targetPatch: MineralPatch
  def requiredWorkers: Int
}