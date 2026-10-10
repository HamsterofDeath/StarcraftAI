package pony
package brain
package modules

class EnqueueFactories(universe: Universe)
    extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  // a builder still on its way counts too: else each tick asks for another one, and the bank locks for all of them
  def ratios = evaluateCapacities.map { cap =>
    cap ->
      (ownUnits.allByClass(cap.typeOfFactory).size +
        unitManager.plannedToBuildByClass(cap.typeOfFactory).size +
        unitManager.constructionsInProgress(cap.typeOfFactory).count(_.couldSwitchInTheFuture))
  }.toMap

  override def onTick_!(): Unit = {

    ratios.foreach { case (cap, existingByType) =>
      if (existingByType < cap.maximumSustainable) {
        requestBuilding(cap.typeOfFactory, takeCareOfDependencies = true, cap.highPriorityNow)
      }
    }
  }

  private def evaluateCapacities = {
    strategy.current.suggestProducers
      .filter(_.isActive)
      .groupBy(_.typeOfFactory)
      .values.map { elems =>
        val sum  = elems.map(_.maximumSustainable).sum
        val copy = elems.head.withNewMaximum(sum)
        copy
      }
  }
}
