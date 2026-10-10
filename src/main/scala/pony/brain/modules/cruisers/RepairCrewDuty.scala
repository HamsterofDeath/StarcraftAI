package pony
package brain
package modules
package cruisers

import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.geometry.MapTilePosition
import pony.units.{Battlecruiser, SCV}

import scala.collection.mutable

/** One SCV of the cruisers' repair crew: it waits at the berth and mends the most hurt cruiser hovering there. */
private[pony] class RepairCrewDuty(worker: SCV, berth: MapTilePosition, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString         = "Cruiser repair crew"
  override def isFinished               = false
  override def jobHasFailedWithoutDeath = false
  override def everyNth                 = 23

  private var mending = Option.empty[Battlecruiser]
  // the hit points of the cruiser being mended when they last rose, and when
  private var progress = (0, 0)
  // cruisers this SCV gave up on, until when
  private val givenUp = mutable.HashMap.empty[Int, Int]

  // One cruiser until it is whole: picking the most hurt anew each time made the crew hop between cruisers mended to
  // the same health, and the hopping left them all stuck at seventy percent. But not one it cannot mend: in the watched
  // game on 28e1ee0 nine SCVs held Repair on the same cruiser, hovering where they could not stand, for half an hour.
  override def ordersForTick = {
    val now                       = currentTick
    def hp(c: Battlecruiser)      = c.nativeUnit.getHitPoints
    def atBerth(c: Battlecruiser) = c.isInGame && !c.isBeingCreated && c.isDamaged &&
      c.currentTile.distanceToIsLess(berth, 8) && !givenUp.contains(c.nativeUnitId)
    givenUp.filterInPlace((_, until) => now < until)
    mending.filter(atBerth).foreach { c =>
      if (hp(c) > progress._1) progress = (hp(c), now)
      else if (CruiserTactics.crewStalled(now, progress._2)) {
        givenUp(c.nativeUnitId) = now + CruiserTactics.CrewGiveUpFrames
        mending = None
      }
    }
    mending = mending.filter(atBerth).orElse {
      val next = ownUnits.allByType[Battlecruiser].filter(atBerth).minByOpt(_.percentageHPOk)
      next.foreach(c => progress = (hp(c), now))
      next
    }
    mending.map(c => Orders.RepairUnit(worker, c)).orElse {
      Option.when(worker.currentTile.distanceToIsMore(berth, 3))(Orders.MoveToTile(worker, berth))
    }.toSeq
  }
}
