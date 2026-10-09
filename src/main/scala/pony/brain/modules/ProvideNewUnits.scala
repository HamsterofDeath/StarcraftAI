package pony
package brain
package modules

class ProvideNewUnits(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  self =>

  override def onTick_!(): Unit = {
    unitManager.failedToProvideFlat.distinct.foreach { req =>
      trace(s"Trying to satisfy $req somehow")
      val wantedType = req.typeOfRequestedUnit
      if (classOf[Mobile] >= wantedType) {
        val typeFixed     = wantedType.asInstanceOf[Class[Mobile]]
        val wantedAmount  = req.amount
        var skipRemaining = false
        (1 to wantedAmount).iterator.takeWhile(_ => !skipRemaining) foreach { _ =>
          val builderOf = UnitJobRequest.builderOf(typeFixed, self)
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
