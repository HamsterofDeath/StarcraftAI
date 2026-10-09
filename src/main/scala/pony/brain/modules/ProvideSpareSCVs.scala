package pony
package brain
package modules

class ProvideSpareSCVs(universe: Universe) extends OrderlessAIModule[CommandCenter](universe) {

  private val emp = new Employer[WorkerUnit](universe)

  override def onTick_!() = {
    super.onTick_!()
    ifNth(Primes.prime37) {
      // only for terran!
      val mining   = universe.pluginByType[ManageMiningAtBases]
      val target   = mining.workerCapacity + universe.pluginByType[ManageMiningAtGeysirs].workerCapacity + 2
      val workers  = ownUnits.allByType[WorkerUnit].filter(_.isInGame).toVector
      val reserved = unitManager.allJobsByType[TrainUnit[UnitFactory, Mobile]].count { job =>
        !job.failedOrObsolete && !job.isFinished && classOf[WorkerUnit] >= job.requestedType
      }
      val requests = unitManager.plannedToTrain.filter(r => classOf[WorkerUnit] >= r.typeOfRequestedUnit)
        .map(_.amount).toVector
      val missing = WorkerProductionQuota.missing(
        target,
        workers.count(!_.isBeingCreated),
        workers.count(_.isBeingCreated),
        reserved,
        ownUnits.allByType[CommandCenter].count(_.nativeUnit.isTraining),
        requests
      )
      if (currentTick < 6000 || currentTick % 2048 == 0) NativeMatchEvidence.trace(
        "spare-scv-scan",
        s"target=$target done=${workers.count(!_.isBeingCreated)} incomplete=${workers.count(_.isBeingCreated)} reserved=$reserved nativeTraining=${ownUnits.allByType[CommandCenter].count(_.nativeUnit.isTraining)} requests=$requests missing=$missing"
      )
      if (missing > 0) {
        val result = unitManager.request(UnitJobRequest.idleOfType(emp, classOf[WorkerUnit], missing))
        if (currentTick < 6000) NativeMatchEvidence.trace(
          "spare-scv-request",
          s"missing=$missing result=${result.getClass.getSimpleName} units=${result.units.size}"
        )
      }
    }
  }
}
