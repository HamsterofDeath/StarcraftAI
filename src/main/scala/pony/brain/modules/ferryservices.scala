package pony
package brain.modules

import pony.brain.{Employer, HasUniverse, UnitJobRequests, Universe}

import scala.collection.mutable.ArrayBuffer

class FerryManager(override val universe: Universe) extends HasUniverse {

  private val ferryPlans = ArrayBuffer.empty[FerryPlan]

  private val employer = new Employer[TransporterUnit](universe)

  def planFor(ferry: TransporterUnit) = {
    ferryPlans.find(_.ferry == ferry)
  }

  def requestFerry(forWhat: GroundUnit, to: MapTilePosition, buildNewIfRequired: Boolean = false) = {
    val job = ferryPlans.find(_.covers(forWhat, to))
              .orElse(requestFerriesAddPlan_!(forWhat.toSet, to, buildNewIfRequired).headOption)
    job.foreach(_.notifyRequested())
    job

  }

  private def requestFerriesAddPlan_!(forWhat: Set[GroundUnit], to: MapTilePosition,
                                      buildNewIfRequired: Boolean = false) = {
    assert(forWhat.forall(_.currentArea != mapLayers.rawWalkableMap.areaWhichContainsAsFree(to)),
      s"One of the units is already in the target area")
    val groups = ArrayBuffer.empty[FerryCargoBuilder]
    val remaining = forWhat.toBuffer

    while (remaining.nonEmpty) {
      var open = new FerryCargoBuilder
      remaining.iterator.takeWhile(open.canAdd).foreach(open.add_!)
      remaining --= open.cargo
      groups += open
    }

    val selector = UnitJobRequests.idleOfType(employer, race.transporterClass, groups.size)
                   .acceptOnly_! { tu =>
                     !ferryPlans.exists(_.ferry == tu)
                   }
    val result = unitManager
                 .request(selector, buildNewIfRequired)
    val newPlans = result.ifNotZero(seq => {
      seq.zip(groups).map {
        case (transporter, cargo) => FerryPlan(transporter, cargo.cargo.toSet, to,
          mapLayers.rawWalkableMap.areaWhichContainsAsFree(to))
      }
    }, Nil)
    ferryPlans ++= newPlans
    newPlans
  }

  def onTick(): Unit = {
    ferryPlans.retain(_.unfinished)
  }
}

class FerryCargoBuilder {
  private val myCargo = ArrayBuffer.empty[GroundUnit]
  private var left    = 8

  def cargo = myCargo.toSeq

  def canAdd(g: GroundUnit) = left >= g.transportSize

  def add_!(g: GroundUnit) = {
    assert(canAdd(g))
    myCargo += g
    left -= g.transportSize
  }
}

case class FerryPlan(ferry: TransporterUnit, toTransport: Set[GroundUnit], toWhere: MapTilePosition,
                     targetArea: Option[Grid2D]) {
  private var lastRequest = ferry.currentTick

  def notifyRequested(): Unit = {
    lastRequest = ferry.currentTick
  }

  def orphaned = lastRequest + 48 < ferry.currentTick

  def unfinished = !orphaned && toTransport.exists { gu =>
    gu.currentArea != targetArea || gu.loaded
  }

  def loadedLeft = {
    ferry.hasUnitsLoaded && ferry.currentArea == targetArea
  }

  def needsToReachTarget = ferry.currentArea != targetArea

  def covers(forWhat: GroundUnit, to: MapTilePosition) = {
    toTransport(forWhat) && toWhere == to
  }

  def unloadedLeft = toTransport.exists { e =>
    e.onGround && e.currentArea != targetArea
  }

  assert(toTransport.map(_.transportSize).sum <= 8, s"Too many units for single transport: $toTransport")

}
