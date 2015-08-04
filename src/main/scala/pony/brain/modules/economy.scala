package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProvideNewSupply(universe: Universe) extends AIModule[WorkerUnit](universe) {
  override def ordersForTick: Traversable[UnitOrder] = {
    Nil
  }
}

class ProvideNewUnits(universe: Universe) extends AIModule[Factory](universe) {
  self =>

  override def ordersForTick: Traversable[UnitOrder] = {
    units.failedToProvide.flatMap { reqs =>
      reqs.requests.flatMap { req =>
        trace(s"Trying to satisfy $req somehow")
        val wantedType = req.typeOfRequestedUnit
        if (classOf[Mobile].isAssignableFrom(wantedType)) {
          val typeFixed = wantedType.asInstanceOf[Class[Mobile]]
          val wantedAmount = req.amount
          1 to wantedAmount flatMap { _ =>
            val builderOf = UnitJobRequests.builderOf(typeFixed, self)
            units.request(builderOf) match {
              case any: ExactlyOneSuccess[Factory] =>
                hire(any)
                val order = new TrainUnit(any.onlyOne, typeFixed, self)
                assignJob(order)
                order.ordersForTick
              case _ => Nil
            }
          }
        } else {
          Nil
        }
      }
    }
  }
}

class GatherMinerals(universe: Universe) extends AIModule(universe) {

  private val gatheringJobs = ArrayBuffer.empty[GatherAtBase]

  override def ordersForTick = {
    createJobsForBases()
    gatheringJobs.flatMap(_.ordersForTick)
  }

  private def createJobsForBases(): Unit = {
    val add = universe.bases.bases.filterNot(e => gatheringJobs.exists(_.covers(e))).flatMap { base =>
      base.myMineralGroup.map { minerals =>
        new GatherAtBase(base, minerals)
      }
    }
    if (add.nonEmpty) {
      info(
        s"""
           |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin)
    }

    gatheringJobs ++= add
  }

  class GatherAtBase(base: Base, minerals: MineralPatchGroup) extends Employer[WorkerUnit](universe) {
    emp =>

    def ordersForTick = {
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        val workerType = base.mainBuilding.race.workerClass
        val result = universe.units.request(UnitJobRequests.idleOfType(emp, workerType, missing))
        hire(result)
      }
      idleUnits.foreach { u =>
        Micro.MiningOrganization.findBestPatch(u).foreach { suggestion =>
          suggestion.addToTeam(u)
        }
      }

      Micro.MiningOrganization.orders
    }

    private def idealNumberOfWorkers = Micro.MiningOrganization.idealNumberOfWorkers

    def covers(aBase: Base) = base == aBase

    override def toString = s"Gathering $minerals at $base"

    object Micro {

      class MinedPatch(val patch: MineralPatch) {
        private val miningTeam = ArrayBuffer.empty[GatherMineralsAtPatch]
        def hasOpenSpot: Boolean = miningTeam.size < estimateRequiredWorkers
        def openSpotCount = estimateRequiredWorkers - miningTeam.size
        def estimateRequiredWorkers = 2
        def orders = miningTeam.flatMap(_.ordersForTick)
        def addToTeam(worker: WorkerUnit): Unit = {
          val job = new GatherMineralsAtPatch(worker, this)
          miningTeam += job
          assignJob(job)
        }

        def removeFromTeam(worker: WorkerUnit): Unit = {
          miningTeam.find(_.unit == worker).foreach {miningTeam -= _}
        }
      }

      class GatherMineralsAtPatch(worker: WorkerUnit, miningTarget: MinedPatch)
        extends UnitWithJob(emp, worker, Priority.Default) {

        import States._

        private var state: State = Idle

        override def ordersForTick: Seq[UnitOrder] = {
          def sendWorkerToPatch = ApproachingMinerals -> Orders.Gather(worker, miningTarget.patch)
          val (newState, order) = state match {
            case Idle if worker.isCarryingMinerals =>
              ReturningMinerals -> Orders.ReturnMinerals(worker, base.mainBuilding)
            // start going to your patch using wall hack
            case Idle if !worker.isCarryingMinerals =>
              sendWorkerToPatch
            // keep going while mining has not been started

            case ApproachingMinerals if miningTarget.patch.isBeingMined =>
              // repeat the order to prevent the worker from moving away
              sendWorkerToPatch

            case ApproachingMinerals if worker.isWaitingForMinerals || worker.isInMiningProcess =>
              // let the poor worker alone now
              Mining -> Orders.NoUpdate

            case ApproachingMinerals =>
              ApproachingMinerals -> Orders.NoUpdate

            case Mining if worker.isInMiningProcess =>
              // let it work
              Mining -> Orders.NoUpdate
            case Mining if worker.isCarryingMinerals =>
              // the worker is done mining
              ReturningMinerals -> Orders.ReturnMinerals(worker, base.mainBuilding)

            case ReturningMinerals if worker.isCarryingMinerals =>
              // keep going back
              ReturningMinerals -> Orders.NoUpdate
            case ReturningMinerals if !worker.isCarryingMinerals =>
              // switch back to mining mode
              sendWorkerToPatch

            case _ =>
              Idle -> Orders.NoUpdate
          }
          state = newState
          order.toList.filterNot(_.isNoop)
        }
        override def finished = false
      }

      object MiningOrganization {
        private val assignments = mutable.HashMap.empty[MineralPatch, MinedPatch]
        def orders = assignments.valuesIterator.flatMap(_.orders)
        minerals.patches.foreach { mp =>
          assignments.put(mp, new MinedPatch(mp))
        }
        def findBestPatch(worker: WorkerUnit) = {
          val maxFreeSlots = assignments.iterator.map(_._2.openSpotCount).max
          val notFull = assignments.filter(_._2.openSpotCount == maxFreeSlots)
          if (notFull.nonEmpty) {
            val (_, patch) = notFull.minBy { case (minerals, _) =>
              minerals.position.distanceTo(worker.currentPosition)
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
      }
    }
  }

}