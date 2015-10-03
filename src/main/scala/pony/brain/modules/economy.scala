package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProvideNewBuildings(universe: Universe)
  extends AIModule[WorkerUnit](universe) with ComputationIntensive[WorkerUnit] {
  self =>

  override type ComputationInput = Data

  override def evaluateNextOrders(in: ComputationInput) = {
    val helper = in.helper
    val job = {
      def newJob(where: MapTilePosition) =
        new ConstructBuilding(in.worker, in.buildingType, self, where, in.jobRequest.proofForFunding.assumeSuccessful,
          in.jobRequest.belongsTo)

      val customPosition = in.jobRequest.customPosition.predefined
                           .orElse(in.jobRequest.customPosition.evaluateCostly)
                           .orElse(helper.findSpotFor(in.base.mainBuilding.tilePosition, in.buildingType))
      customPosition.map(newJob)
    }
    job match {
      case None =>
        // TODO fix it if it ever happens
        error(s"Computation returned no result: $in")
        BackgroundComputationResult.nothing[WorkerUnit](() => {
          if (in.jobRequest.stillLocksResources) {
            in.jobRequest.forceUnlockOnDispose_!()
            in.jobRequest.dispose()
          } else {
            warn(s"Why is the same request processed again?")
          }
        })
      case Some(startJob) =>
        BackgroundComputationResult.result(startJob.toSeq, () => false, () => {
          info(s"Planning to build ${in.buildingType.className} at ${startJob.buildWhere} by ${in.worker}")
          in.jobRequest.clearableInNextTick_!()
          in.jobRequest.doOnDispose_! {
            mapLayers.unblockBuilding_!(startJob.area)
          }
          mapLayers.blockBuilding_!(startJob.area)
          //maybe there is a better worker for this than the one that was initially chosen
          unitManager.tryFindBetterEmployeeFor(startJob)
        })
    }
  }

  override def calculationInput = {
    // we do them one by one, it's simpler
    val buildingRelated = unitManager.failedToProvideByType[Building]
    val constructionRequests = buildingRelated.collect {
      case buildIt: BuildUnitRequest[Building] if buildIt.proofForFunding.isFunded && !buildIt.isAddon =>
        buildIt
    }
    val anyOfThese = constructionRequests.headOption
    anyOfThese.flatMap { req =>
      val request = UnitJobRequests.constructor(self)
      unitManager.request(request) match {
        case success: ExactlyOneSuccess[WorkerUnit] =>
          req.customPosition.init_!()
          new Data(success.onlyOne, req.typeOfRequestedUnit,
            unitManager.bases.mainBase.getOr("All your base are belong to us"),
            new ConstructionSiteFinder(universe), req).toSome
        case _ => None
      }
    }
  }

  case class Data(worker: WorkerUnit, buildingType: Class[_ <: Building], base: Base, helper: ConstructionSiteFinder,
                  jobRequest: BuildUnitRequest[Building])
}

class ProvideExpansions(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private var plannedExpansionPoint = Option.empty[ResourceArea]
  def forceExpand(patch: MineralPatchGroup) = {
    plannedExpansionPoint = {
      val all = strategicMap.domains.flatMap(_._2.values.flatten)
      all.find(_.isPatchId(patch.patchId))
    }
  }
  override def onTick(): Unit = {
    ifNth(127) {
      plannedExpansionPoint = plannedExpansionPoint.orElse(strategy.current.suggestNextExpansion)
      info(s"AI wants to expand to ${plannedExpansionPoint.get}", plannedExpansionPoint.isDefined)
    }

    plannedExpansionPoint.foreach { resources =>
      val plannedMainBuildings = unitManager.constructionsInProgress(race.resourceDepositClass)
      plannedMainBuildings.find(_.belongsTo.contains(resources)) match {
        case Some(planned) =>
          if (planned.building.isDefined) {
            plannedExpansionPoint = None
          }
        case None =>
          if (!unitManager.plannedToBuild(race.resourceDepositClass)) {
            val buildingSpot = AlternativeBuildingSpot
                               .fromExpensive(new ConstructionSiteFinder(universe).forResourceArea(resources))(_.find)
            requestBuilding(race.resourceDepositClass, takeCareOfDependencies = false, saveMoneyIfPoor = true,
              buildingSpot, belongsTo = plannedExpansionPoint, priority = Priority.Expand)
          }
      }
    }
  }
}

class ProvideNewSupply(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) {
  private val supplyEmployer = new Employer[SupplyProvider](universe)
  override def onTick() = {

    val cur = plannedSupplies
    val needsMore = cur.supplyUsagePercent >= 0.8 && cur.total < 400

    trace(s"Need more supply: $cur ($plannedSupplies planned)", needsMore)
    if (needsMore) {
      // can't use helper because overlords are not buildings :|
      val race = universe.myRace
      val result = resources.request(ResourceRequests.forUnit(race, classOf[SupplyProvider], Priority.Supply), this)
      result.ifSuccess { suc =>
        trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
        val ofType = UnitJobRequests
                     .newOfType(universe, supplyEmployer, classOf[SupplyProvider], suc, priority = Priority.Supply)

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

class ProvideNewUnits(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  self =>

  override def onTick(): Unit = {
    unitManager.failedToProvideFlat.flatMap { req =>
      trace(s"Trying to satisfy $req somehow")
      val wantedType = req.typeOfRequestedUnit
      if (classOf[Mobile].isAssignableFrom(wantedType)) {
        val typeFixed = wantedType.asInstanceOf[Class[Mobile]]
        val wantedAmount = req.amount
        1 to wantedAmount flatMap { _ =>
          val builderOf = UnitJobRequests.builderOf(typeFixed, self)
          unitManager.request(builderOf) match {
            case any: ExactlyOneSuccess[UnitFactory] =>
              val producer = any
              unitManager.jobOf(producer.onlyOne) match {
                case t: CreatesUnit[_] =>
                  req.clearableInNextTick_!()
                  Nil
                case _ =>
                  resources.request(ResourceRequests.forUnit(universe.myRace, typeFixed, req.priority), self) match {
                    case suc: ResourceApprovalSuccess =>
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

class DefaultBehaviours(universe: Universe) extends OrderlessAIModule[Mobile](universe) {
  self =>
  private val rules  = Terran.allBehaviours(universe)
  private val ignore = mutable.HashSet.empty[Mobile]

  override def renderDebug(renderer: Renderer): Unit = {
    rules.foreach(_.renderDebug(renderer))
  }
  override def onTick(): Unit = {
    rules.foreach(_.onTick())
    // fetch all idles and assign "always on" background tasks to them
    val hireUs = unitManager.allIdleMobiles.filterNot(e => ignore(e.unit)).flatMap { free =>
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

class GatherMinerals(universe: Universe) extends OrderlessAIModule(universe) {

  private val gatheringJobs = ArrayBuffer.empty[GetMinerals]

  override def onTick(): Unit = {
    createJobsForBases()
    gatheringJobs.foreach(_.onTick())
  }

  private def createJobsForBases(): Unit = {
    val add = universe.bases.bases
              .filterNot(e => gatheringJobs.exists(_.covers(e)))
              .filterNot(_.mainBuilding.isBeingCreated)
              .flatMap { base =>
                base.myMineralGroup.map { minerals =>
                  new GetMinerals(base, minerals)
                }
              }
    info(
      s"""
         |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin, add.nonEmpty)
    gatheringJobs ++= add
  }

  class GetMinerals(base: Base, minerals: MineralPatchGroup) extends Employer[WorkerUnit](universe) {
    emp =>

    def onTick(): Unit = {
      minerals.tick()
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        val result = universe.unitManager.request(UnitJobRequests.idleOfType(emp, classOf[WorkerUnit], missing))
        val jobs = result.units.flatMap { worker =>
          Micro.MiningOrganization.findBestPatch(worker).map { patch =>
            info(s"Added $worker to mining team of $patch")
            val job = new Micro.GatherMineralsAtPatch(worker, patch)
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

        private val miningTeam            = ArrayBuffer.empty[GatherMineralsAtPatch]
        private val workerCountByDistance = LazyVal
                                            .from(math.round(patch.area.distanceTo(base.mainBuilding.area) / 2.0).toInt)
        override def toString: String = s"(Mined) $patch"
        def hasOpenSpot: Boolean = miningTeam.size < estimateRequiredWorkers
        def estimateRequiredWorkers = {
          if (patch.remainingMinerals > 0) workerCountByDistance.get else 0
        }
        def openSpotCount = estimateRequiredWorkers - miningTeam.size
        def lockToPatch_!(job: GatherMineralsAtPatch): Unit = {
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

      class GatherMineralsAtPatch(myWorker: WorkerUnit, miningTarget: MinedPatch)
        extends UnitWithJob(emp, myWorker, Priority.Default) with GatherMineralsAtSinglePatch with Interruptable {

        listen_!(failed => {
          if (miningTarget.isInTeam(myWorker)) {
            miningTarget.removeFromPatch_!(myWorker)
          }
        })

        import States._

        private var state: State = Idle
        override def onStealUnit(): Unit = {
          super.onStealUnit()
          miningTarget.removeFromPatch_!(myWorker)
        }
        override def worker = myWorker
        override def targetPatch = miningTarget.patch
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

            case ApproachingMinerals if myWorker.isWaitingForMinerals || myWorker.isInMiningProcess =>
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

            case ReturningMineralsAfterInterruption if myWorker.isCarryingMinerals && !myWorker.isMoving =>
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
        override def isFinished = miningTarget.patch.remainingMinerals <= 0

        override def hasFailed: Boolean = super.hasFailed || base.mainBuilding.isDead
      }

      object MiningOrganization {
        private val assignments = mutable.HashMap.empty[MineralPatch, MinedPatch]

        minerals.patches.foreach { mp =>
          assignments.put(mp, new MinedPatch(mp))
        }

        def findBestPatch(worker: WorkerUnit) = {
          val maxFreeSlots = assignments.iterator.map(_._2.openSpotCount).max
          val notFull = assignments.filter(_._2.openSpotCount == maxFreeSlots)
          if (notFull.nonEmpty) {
            val (_, patch) = notFull.minBy { case (mins, _) =>
              mins.area.closestDirectConnection(base.mainBuilding).length
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

class GatherGas(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private val gatheringJobs = ArrayBuffer.empty[GetGas]
  override def onTick(): Unit = {
    val unattended = unitManager.bases.bases.filter(base => !gatheringJobs.exists(_.covers(base)))
    unattended.foreach { base =>
      base.myGeysirs.map { geysir =>
        new GetGas(base, geysir)
      }.foreach {gatheringJobs += _}
    }

    gatheringJobs.foreach(_.onTick())
  }

  class GetGas(base: Base, geysir: Geysir) extends Employer[WorkerUnit](universe) {
    self =>
    private val idealWorkerCount            = 3 + (base.mainBuilding.area.distanceTo(geysir.area) / 3).toInt
    private val workerCountBeforeWantingGas = universe.mapLayers
                                              .isOnIsland(base.mainBuilding.tilePosition)
                                              .ifElse(8, 14)
    private var refinery                    = Option.empty[Refinery]

    override def toString = s"GetGas@${geysir.tilePosition}"

    def onTick(): Unit = {
      refinery match {
        case None =>
          if (unitManager.unitsByType[WorkerUnit].size >= workerCountBeforeWantingGas) {
            def requestExists = unitManager.requestedConstructions[Refinery]
                                .exists(_.customPosition.predefined.contains(geysir.tilePosition))
            def jobExists = unitManager.constructionsInProgress[Refinery]
                            .exists(_.buildWhere == geysir.tilePosition)
            if (!requestExists && !jobExists) {
              val where = AlternativeBuildingSpot.fromPreset(geysir.tilePosition)
              requestBuilding(classOf[Refinery], customBuildingPosition = where)
            }
            unitManager.unitsByType[Refinery]
            .find(_.tilePosition == geysir.tilePosition)
            .filterNot(_.isBeingCreated)
            .foreach { refinery =>
              self.refinery = Some(refinery)
            }
          }
        case Some(ref) =>
          val missing = idealWorkerCount - teamSize
          val ofType = UnitJobRequests.idleOfType(self, classOf[WorkerUnit], missing, Priority.CollectGas)
                       .acceptOnly_!(_.isCarryingNothing)
          val result = unitManager.request(ofType)
          result.units.foreach { freeWorker =>
            assignJob_!(new GatherGasAtRefinery(freeWorker))
          }
      }
    }
    def covers(base: Base) = this.base.mainBuilding == base.mainBuilding

    class GatherGasAtRefinery(worker: WorkerUnit)
      extends UnitWithJob[WorkerUnit](self, worker, Priority.ConstructBuilding) with Interruptable {

      private var state: State = Idle
      override def shortDebugString: String = state.getClass.className
      override def isFinished = false
      override protected def ordersForTick: Seq[UnitOrder] = {
        val (newState, order) = state match {
          case Idle =>
            Gathering -> Orders.Gather(worker, geysir)
          case Gathering if worker.isGatheringGas || worker.isMagic =>
            Gathering -> Orders.NoUpdate(worker)
          case _ =>
            Idle -> Orders.NoUpdate(worker)
        }
        state = newState
        order.toSeq
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