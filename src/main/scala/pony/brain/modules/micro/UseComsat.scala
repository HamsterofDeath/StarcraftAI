package pony
package brain
package modules
package micro

import pony.geometry.MapTilePosition
import pony.units.Comsat

import pony.tech.Upgrades.Terran._

import scala.collection.mutable

/**
  * Scanner sweeps only where a hidden enemy is known to be and can be killed once revealed, never on a guess
  * (WatchForCloak): a cloaked or burrowed enemy the game shows near our units that can hit it, attackers first, then
  * detectors such as observers; or a unit or building of ours losing hit points with nothing visible near that could
  * have done it, nor our own fire. A sweep lasts about eleven seconds, so a spot just swept is not swept again.
  */
class UseComsat(universe: Universe) extends DefaultBehaviour[Comsat](universe) {
  import ScanChoice._

  private val sweeps = mutable.ArrayBuffer.empty[(MapTilePosition, Int)]
  private var wanted = Option.empty[Candidate]

  override def onTick_!() = {
    super.onTick_!()
    val now = currentTick
    sweeps.filterInPlace((_, at) => now - at < SweepFrames)
    wanted = choose(universe.pluginByType[WatchForCloak].candidates, sweeps.toSeq.map(_._1))
  }

  override protected def wrapBase(comsat: Comsat) = new SingleUnitBehaviour[Comsat](comsat, meta) {
    override def describeShort = "Scan"

    override def toOrder(what: Objective) = wanted.filter(_ => comsat.canCastNow(ScannerSweep)).map { c =>
      wanted = None
      sweeps += c.at -> currentTick
      NativeMatchEvidence.trace("comsat-sweep", s"reason=${c.reason} at=${c.at} hunters=${c.hunters} about=${c.about}")
      Orders.ScanWithComsat(comsat, c.at)
    }.toList
  }
}
