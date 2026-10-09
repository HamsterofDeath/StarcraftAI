package pony
package brain
package modules

import scala.jdk.CollectionConverters._

class ProvideNewBuildings(universe: Universe)
    extends AIModule[WorkerUnit](universe) with BackgroundComputation[WorkerUnit] {
  self =>

  override type ComputationInput = Data

  // Construction planning takes milliseconds but waited seconds in the shared pool behind path and attack planning,
  // so a single building every half minute was started; it gets a thread of its own.
  override protected def computationContext                         = ProvideNewBuildings.planner
  protected def constructionSite(in: Data): Option[MapTilePosition] =
    in.jobRequest.customPosition.resolve(in.helper.findSpotFor(in.mainBuildingwhere, in.buildingType))

  override def evaluateNextOrders(in: ComputationInput) = {
    val helper = in.helper
    val job    = {
      def newJob(where: MapTilePosition) =
        new ConstructBuilding(
          in.worker,
          in.buildingType,
          self,
          where,
          in.jobRequest.proofForFunding.assumeSuccessful,
          in.jobRequest.belongsTo
        )

      val customPosition = constructionSite(in)

      customPosition
        .foreach(e => assert(mapLayers.rawWalkableMap.insideBounds(e), s"$e was not inside map :("))
      customPosition.map(where => () => newJob(where))
    }

    job match {
      case None =>
        error(s"Computation returned no result: $in")
        BackgroundComputationResult.nothing[WorkerUnit](() => {
          if (in.jobRequest.stillLocksResources) {
            in.jobRequest.clearableInNextTick_!()
            in.jobRequest.forceUnlockOnDispose_!()
          } else {
            warn(s"Why is the same request processed again? -> ${in.jobRequest}")
          }
        })
      case Some(newJobFactory) =>
        BackgroundComputationResult
          .result[WorkerUnit, ConstructBuilding[WorkerUnit, ? <: Building]](
            myJobs = newJobFactory.toSeq,
            checkValidityNow = () => false,
            canCreateNow = () =>
              !in.jobRequest.clearable && in.jobRequest.stillLocksResources &&
                resources.hasStillLocked(in.jobRequest.funding)
          ) { jobs =>
            jobs.headOption.foreach { job =>
              info(s"Planning to build ${in.buildingType.className} at ${job.buildWhere} by ${
                  in.worker
                }")
              in.jobRequest.clearableInNextTick_!()
              mapLayers.blockBuilding_!(job.area)
              // maybe there is a better worker for this than the one that was initially chosen
              unitManager.tryFindBetterEmployeeFor(job)
            }
          }
    }
  }

  override def calculationInput = {
    // we do them one by one, it's simpler. in the next tick, the same missing buildings will
    // still be requested, so the queue will be refilled
    val buildingRelated      = unitManager.failedToProvideByType[Building]
    val constructionRequests = {
      buildingRelated.iterator.collect {
        case buildIt: BuildUnitRequest[Building]
            if buildIt.proofForFunding.isSuccess &&
              !buildIt.clearable &&
              !buildIt.isAddon &&
              universe.resources.hasStillLocked(buildIt.funding) =>
          buildIt
      }
    }

    // A building whose required buildings are not completed yet is refused by BWAPI at its site: a worker would walk
    // there and wait until the job times out. Such requests wait without a worker until the requirements stand.
    def requirementsStand(candidate: BuildUnitRequest[Building]) = {
      val self = nativeGame.self()
      candidate.typeOfRequestedUnit.toUnitType.requiredUnits().asScala.forall { case (required, _) =>
        required.isWorker || self.completedUnitCount(required) > 0
      }
    }
    val anyOfThese = constructionRequests.filter(requirementsStand).find { candidate =>
      val passThrough            = !candidate.isUpgrader
      def isUpgradeEnablerOrRich = {
        val count   = ownUnits.allByClass(candidate.typeOfRequestedUnit).size
        val isFirst = count == 0
        isFirst || bases.rich && bases.allBases.size > count
      }
      passThrough || isUpgradeEnablerOrRich
    }

    anyOfThese.flatMap { req =>
      val request = UnitJobRequest.constructor(self)
      unitManager.request(request) match {
        case success: ExactlyOneSuccess[WorkerUnit] =>
          req.customPosition.init_!()
          val randomWorker = success.onlyOne
          new Data(
            randomWorker,
            req.typeOfRequestedUnit,
            unitManager.bases.mainBase.getOr("All your base are belong to us").mainBuilding.tilePosition,
            new ConstructionSiteFinder(universe),
            req
          ).toSome
        case _ => None
      }
    }
  }

  case class Data(
      worker: WorkerUnit,
      buildingType: Class[? <: Building],
      mainBuildingwhere: MapTilePosition,
      helper: ConstructionSiteFinder,
      jobRequest: BuildUnitRequest[Building]
  ) {
    private val workerId  = worker.nativeUnitId
    override def toString =
      s"ConstructionData(worker=$workerId, building=${buildingType.className}, home=$mainBuildingwhere)"
  }

}

object ProvideNewBuildings {

  /** One daemon thread for every game of this process: construction planning never waits for other planning. */
  val planner: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(
    java.util.concurrent.Executors.newSingleThreadExecutor { (r: Runnable) =>
      val thread = new Thread(r, "construction-planning")
      thread.setDaemon(true)
      thread
    }
  )
}
