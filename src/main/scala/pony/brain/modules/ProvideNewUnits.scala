package pony
package brain
package modules

import scala.jdk.CollectionConverters._

class ProvideNewUnits(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  self =>

  override def onTick_!(): Unit = {
    unitManager.failedToProvideFlat.distinct.foreach { req =>
      trace(s"Trying to satisfy $req somehow")
      val wantedType       = req.typeOfRequestedUnit
      val workerCapReached = classOf[WorkerUnit] >= wantedType &&
        ownUnits.allByType[WorkerUnit].size >= strategy.current.maxWorkers
      if (workerCapReached) req.clearableInNextTick_!()
      else if (classOf[Mobile] >= wantedType) {
        val typeFixed     = wantedType.asInstanceOf[Class[Mobile]]
        val wantedAmount  = req.amount
        var skipRemaining = false
        (1 to wantedAmount).iterator.takeWhile(_ => !skipRemaining) foreach { _ =>
          // a unit that needs its producer's add-on (a Battlecruiser its Starport's Control Tower) goes only to one
          // that has it: another refuses the order, gives up after 25 frames and may be picked again
          val nativeType            = TypeMapping.unitTypeOf(typeFixed)
          val needsAddon            = nativeType.requiredUnits.asScala.keys.exists(_.isAddon)
          def ready(f: UnitFactory) = !needsAddon || f.nativeUnit.canTrain(nativeType)
          val builderOf             = req.trainNear.fold(
            UnitJobRequest.builderOf(typeFixed, self).withRequest(_.withFilter_!(ready))
          ) { near =>
            // a worker for one base comes from that base: one trained elsewhere may never walk there
            UnitJobRequest.builderOf(typeFixed, self).withRequest(
              _.withFilter_!(f => ready(f) && !ferryManager.sealedApart(f.tilePosition, near))
                .withCherryPicker_!(job => PriorityChain(job.unit.tilePosition.distanceSquaredTo(near).toDouble))
            )
          }
          unitManager.request(builderOf) match {
            case producer: ExactlyOneSuccess[UnitFactory] =>
              unitManager.jobOf(producer.onlyOne) match {
                case t: CreatesUnit[?] =>
                  req.clearableInNextTick_!()
                  skipRemaining = true
                case _ =>
                  val res = req match {
                    case hf: HasFunding if resources.hasStillLocked(hf.proofForFunding) =>
                      hf.proofForFunding

                    case _ =>
                      val forUnit = ResourceRequests
                        .forUnit(universe.forces.myself.scRace, typeFixed, req.priority)
                      resources.request(forUnit, self)
                  }
                  res match {
                    case suc: ResourceApprovalSuccess =>
                      // job will take care of resource disposal
                      req.keepResourcesLocked_!()
                      req.clearableInNextTick_!()
                      val order = new TrainUnit(producer.onlyOne, typeFixed, self, suc)
                      assignJob_!(order)
                    case _ =>
                      req.clearableInNextTick_!()
                      skipRemaining = true
                  }
              }
            case _ =>
              req.clearableInNextTick_!()
              skipRemaining = true
          }
        }
      }
    }
  }
}
