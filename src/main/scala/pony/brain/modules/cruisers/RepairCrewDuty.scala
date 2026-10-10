package pony
package brain
package modules
package cruisers

import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.geometry.MapTilePosition
import pony.units.{Battlecruiser, SCV}

/** One SCV of the cruisers' repair crew: it waits at the berth and mends the most hurt cruiser hovering there. */
private[pony] class RepairCrewDuty(worker: SCV, berth: MapTilePosition, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString         = "Cruiser repair crew"
  override def isFinished               = false
  override def jobHasFailedWithoutDeath = false
  override def everyNth                 = 23

  private var mending = Option.empty[Battlecruiser]

  // One cruiser until it is whole: picking the most hurt anew each time made the crew hop between cruisers mended to
  // the same health, and the hopping left them all stuck at seventy percent.
  override def ordersForTick = {
    def atBerth(c: Battlecruiser) =
      c.isInGame && !c.isBeingCreated && c.isDamaged && c.currentTile.distanceToIsLess(berth, 8)
    mending =
      mending.filter(atBerth).orElse(ownUnits.allByType[Battlecruiser].filter(atBerth).minByOpt(_.percentageHPOk))
    mending.map(c => Orders.RepairUnit(worker, c)).orElse {
      Option.when(worker.currentTile.distanceToIsMore(berth, 3))(Orders.MoveToTile(worker, berth))
    }.toSeq
  }
}
