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
    val helper = new ConstructionSiteFinder(universe)
    val job = helper.findSpotFor(in.base.mainBuilding.tilePosition, in.buildingType).map { where =>
      new ConstructBuilding(in.worker, in.buildingType, self, where, in.jobRequest.proofForFunding.assumeSuccessful)
    }
    job match {
      case None =>
        // TODO fix it if it ever happens
        error(s"Computation returned no result: $in")
        BackgroundComputationResult.nothing[WorkerUnit]
      case Some(startJob) =>
        BackgroundComputationResult.result(startJob.toSeq, () => false, () => {
          info(s"Planning to build ${in.buildingType.className} at ${startJob.where} by ${in.worker}")
          in.jobRequest.clearableInNextTick_!()
          mapLayers.blockBuilding_!(startJob.area)
        })
    }
  }

  override def calculationInput = {
    // we do them one by one, it's simpler
    val buildingRelated = unitManager.failedToProvideByType[Building]
    val constructionRequests = buildingRelated.collect {
      case buildIt: BuildUnitRequest[Building] if buildIt.proofForFunding.isFunded => buildIt
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
      val result = resources.request(ResourceRequests.forUnit(race.supplyClass, Priority.Supply), this)
      result match {
        case suc: ResourceApprovalSuccess =>
          trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
          val ofType = UnitJobRequests
                       .newOfType(universe, supplyEmployer, race.supplyClass, suc, priority = Priority.Supply)

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
                  resources.request(ResourceRequests.forUnit(typeFixed, req.priority), self) match {
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

  private val gatheringJobs = ArrayBuffer.empty[GatherAtBase]

  override def onTick(): Unit = {
    createJobsForBases()
    gatheringJobs.foreach(_.onTick())
  }

  private def createJobsForBases(): Unit = {
    val add = universe.bases.bases.filterNot(e => gatheringJobs.exists(_.covers(e))).flatMap { base =>
      base.myMineralGroup.map { minerals =>
        new GatherAtBase(base, minerals)
      }
    }
      info(
        s"""
           |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin, add.nonEmpty)
    gatheringJobs ++= add
  }

  class GatherAtBase(base: Base, minerals: MineralPatchGroup) extends Employer[WorkerUnit](universe) {
    emp =>

    def onTick(): Unit = {
      minerals.tick()
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        val workerType = base.mainBuilding.race.workerClass
        val result = universe.unitManager.request(UnitJobRequests.idleOfType(emp, workerType, missing))
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
        def estimateRequiredWorkers = 2
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
              val (from, to) = {
                mins.area.closestDirectConnection(base.mainBuilding)
              }
              from.distanceTo(to)
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

// need this for instanceof checks
trait GatherMineralsAtSinglePatch extends UnitWithJob[WorkerUnit] {
  def worker: WorkerUnit
  def targetPatch: MineralPatch
}