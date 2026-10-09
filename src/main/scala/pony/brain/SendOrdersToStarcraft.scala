package pony
package brain

class SendOrdersToStarcraft(universe: Universe) extends AIModule[Controllable](universe) {
  override def ordersForTick: Iterable[UnitOrder] = {
    unitManager.allJobsByUnitType[Controllable].filterNot(_.failedOrObsolete).flatMap { job =>
      job.ordersForThisTick
    }
  }
}
