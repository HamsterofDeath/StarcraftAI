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
        new ConstructBuilding(in.worker, in.buildingType, self, where, in.jobRequest.proofForFunding.assumeSuccessful)

      in.jobRequest.forceBuildingPosition
      .orElse(helper.findSpotFor(in.base.mainBuilding.tilePosition, in.buildingType))
      .map(newJob)
    }
    job match {
      case None =>
        // TODO fix it if it ever happens
        error(s"Computation returned no result: $in")
        BackgroundComputationResult.nothing[WorkerUnit]
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
          new Data(success.onlyOne, req.typeOfRequestedUnit, unitManager.bases.mainBase,
            new ConstructionSiteFinder(universe), req).toSome
        case _ => None
      }
    }
  }

  case class Data(worker: WorkerUnit, buildingType: Class[_ <: Building], base: Base, helper: ConstructionSiteFinder,
                  jobRequest: BuildUnitRequest[Building])
}

class ProvideAddons(universe: Universe)
  extends OrderlessAIModule[CanBuildAddons](universe) {
  self =>

  override def onTick(): Unit = {
    val buildingRelated = unitManager.failedToProvideByType[Addon]
    val constructionRequests = buildingRelated.collect {
      case buildIt: BuildUnitRequest[Addon] if buildIt.proofForFunding.isFunded && buildIt.isAddon => buildIt
    }
    constructionRequests.foreach { req =>
      val request = UnitJobRequests.addonConstructor(self)
      unitManager.request(request) match {
        case success: ExactlyOneSuccess[CanBuildAddons] =>
          assignJob_!(new ConstructAddon(self, success.onlyOne, req.typeOfRequestedUnit, req.proofForFunding))
        case _ => None
      }
    }
  }

}

class ProvideExpansions(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) {
  override def onTick(): Unit = {

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
      val race = universe.bases.mainBase.mainBuilding.race
      val result = resources.request(ResourceRequests.forUnit(race, classOf[SupplyProvider], Priority.Supply), this)
      result match {
        case suc: ResourceApprovalSuccess =>
          trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
          val ofType = UnitJobRequests
                       .newOfType(universe, supplyEmployer, classOf[SupplyProvider], suc, priority = Priority.Supply)

          // this will always be unfulfilled
          val result = unitManager.request(ofType)
          assert(!result.success, s"Impossible success: $result")
        case _ =>
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
                  resources.request(ResourceRequests.forUnit(universe.race, typeFixed, req.priority), self) match {
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

class GatherMinerals(universe: Universe) extends OrderlessAIModule(universe) {

  private val gatheringJobs = ArrayBuffer.empty[GetMinerals]

  override def onTick(): Unit = {
    createJobsForBases()
    gatheringJobs.foreach(_.onTick())
  }

  private def createJobsForBases(): Unit = {
    val add = universe.bases.bases.filterNot(e => gatheringJobs.exists(_.covers(e))).flatMap { base =>
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

        private val miningTeam = ArrayBuffer.empty[GatherMineralsAtPatch]
        override def toString: String = s"(Mined) $patch"
        def hasOpenSpot: Boolean = miningTeam.size < estimateRequiredWorkers
        def estimateRequiredWorkers = math.round(patch.area.distanceTo(base.mainBuilding.area) / 2.0).toInt
        def openSpotCount = estimateRequiredWorkers - miningTeam.size
        def lockToPatch_!(job: GatherMineralsAtPatch): Unit = {
          info(s"Added ${job.unit} to mining team of $patch")
          miningTeam += job
        }

        def removeFromPatch_!(worker: WorkerUnit): Unit = {
          info(s"Removing $worker from mining team of $patch")
          val found = miningTeam.find(_.unit == worker)
          assert(found.isDefined, s"Did not find $worker in $miningTeam")
          found.foreach {miningTeam -= _}
        }
      }

      class GatherMineralsAtPatch(myWorker: WorkerUnit, miningTarget: MinedPatch)
        extends UnitWithJob(emp, myWorker, Priority.Default) with GatherMineralsAtSinglePatch {

        listen_!(() => {
          miningTarget.removeFromPatch_!(myWorker)
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
              Mining -> Orders.NoUpdate

            case ApproachingMinerals =>
              ApproachingMinerals -> Orders.NoUpdate

            case Mining if myWorker.isInMiningProcess =>
              // let it work
              Mining -> Orders.NoUpdate
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
              ReturningMinerals -> Orders.NoUpdate
            case ReturningMinerals if !myWorker.isCarryingMinerals =>
              // switch back to mining mode
              sendWorkerToPatch

            case _ =>
              Idle -> Orders.NoUpdate
          }
          state = newState
          order.toList.filterNot(_.isNoop)
        }
        override def isFinished = false
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
    private val idealWorkerCount            = 4 + (base.mainBuilding.area.distanceTo(geysir.area) / 3).toInt
    private val workerCountBeforeWantingGas = universe.mapLayers
                                              .isOnIsland(base.mainBuilding.tilePosition)
                                              .ifElse(8, 14)
    private var refinery                    = Option.empty[Refinery]

    override def toString = s"GetGas@${geysir.tilePosition}"

    def onTick(): Unit = {
      refinery match {
        case None =>
          if (unitManager.unitsByType[WorkerUnit].size >= workerCountBeforeWantingGas) {
            def requestExists = unitManager.plannedConstructions[Refinery]
                                .exists(_.forcedPosition.contains(geysir.tilePosition))
            def jobExists = unitManager.constructionsInProgress[Refinery]
                            .exists(_.buildWhere == geysir.tilePosition)
            if (!requestExists && !jobExists) {
              requestBuilding(classOf[Refinery], false, Some(geysir.tilePosition))
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
      extends UnitWithJob[WorkerUnit](self, worker, Priority.ConstructBuilding) {

      private var state: State = Idle
      override def shortDebugString: String = state.getClass.className
      override def isFinished = false
      override protected def ordersForTick: Seq[UnitOrder] = {
        val (newState, order) = state match {
          case Idle =>
            Gathering -> Orders.Gather(worker, geysir)
          case Gathering if worker.isGatheringGas || worker.isMagic =>
            Gathering -> Orders.NoUpdate
          case _ =>
            Idle -> Orders.NoUpdate
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