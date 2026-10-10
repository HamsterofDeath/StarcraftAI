package pony
package brain

import pony.units.Controllable

class SendOrdersToStarcraft(universe: Universe) extends AIModule[Controllable](universe) {
  override def ordersForTick: Iterable[UnitOrder] = {
    unitManager.allJobsByUnitType[Controllable].filterNot(_.failedOrObsolete).flatMap { job =>
      CpuProfile.time("job:" + job.getClass.getName.split('.').last)(job.ordersForThisTick.toVector)
    }
  }
}
