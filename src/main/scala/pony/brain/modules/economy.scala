package pony
package brain
package modules

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GatherMinerals(override val universe: Universe) extends AIModule {

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
    info(
      s"""
         |Added new mineral gathering job(s): ${add.mkString(" & ")}
       """.stripMargin)

    gatheringJobs ++= add
  }
  class GatherAtBase(base: Base, minerals: MineralPatchGroup) extends Employer(universe.units) {
    emp =>

    def ordersForTick = {
      val missing = idealNumberOfWorkers - teamSize
      if (missing > 0) {
        val result = universe.units.request(UnitJobRequests.idleOfType(emp, classOf[WorkerUnit]))
        hire(result)
      }
      idleUnits.foreach { u =>
        Micro.MiningOrganization.findBestPatch(u.asInstanceOf[WorkerUnit]).foreach { suggestion =>
          suggestion.addToTeam(u.asInstanceOf[WorkerUnit])
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
        def hasOpenSpot: Boolean = current.size < estimateRequiredWorkers
        def estimateRequiredWorkers = 2
        def orders = miningTeam.iterator.flatMap(_.ordersForTick)
        def addToTeam(worker: WorkerUnit): Unit = {
          miningTeam += new GatherMineralsAtPatch(worker, this)
        }

        def removeFromTeam(worker: WorkerUnit): Unit = {
          val elem = miningTeam.find(_.unit == worker).foreach {miningTeam -= _}
        }
      }
      class GatherMineralsAtPatch(worker: WorkerUnit, miningTarget: MinedPatch) extends UnitWithJob(emp, worker) {

        import States._

        private var state: State = Idle

        override def ordersForTick: Seq[Order] = {
          def sendWorkerToPatch = ApproachingMinerals -> Orders.Gather(worker, miningTarget.patch)
          val (newState, order) = state match {
            case Idle if worker.isCarryingMinerals =>
              ReturningMinerals -> Orders.ReturnMinerals(worker, base.mainBuilding)
            // start going to your patch using wall hack
            case Idle if !worker.isCarryingMinerals =>
              sendWorkerToPatch
            // keep going while mining has not been started
            case ApproachingMinerals if !worker.isMining =>
              // repeat the order to prevent the worker from moving away
              sendWorkerToPatch
            case ApproachingMinerals if worker.isMining =>
              // let the poor worker alone now
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
      }

      object MiningOrganization {
        private val assignments = mutable.HashMap.empty[MineralPatch, MinedPatch]
        def orders = assignments.valuesIterator.flatMap(_.orders)
        minerals.patches.foreach { mp =>
          assignments.put(mp, new MinedPatch(mp))
        }
        def findBestPatch(worker: WorkerUnit) = {
          val notFull = assignments.filter(_._2.hasOpenSpot)
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