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
    trace(s"Requested ferry for $forWhat to go to $to")
    val job = ferryPlans.find { plan =>
      def canAdd = {
        val area = mapLayers.rawWalkableMap.areaWhichContainsAsFree(to)
        plan.targetArea == area && plan.hasSpaceFor(forWhat)
      }
      if (plan.covers(forWhat)) {
        true
      } else if (canAdd) {
        plan.withMore_!(forWhat)
        true
      } else {
        false
      }
    }.orElse(requestFerriesAddPlan_!(forWhat.toSet, to, buildNewIfRequired).headOption)
    job.foreach(_.notifyRequested_!())
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
    trace(s"Calculating new ferry job for $forWhat to $to")

    val selector = UnitJobRequests.idleOfType(employer, race.transporterClass, groups.size)
                   .acceptOnly_! { tu =>
                     !ferryPlans.exists(_.ferry == tu)
                   }
    val result = unitManager
                 .request(selector, buildNewIfRequired)
    val newPlans = result.ifNotZero(seq => {
      seq.zip(groups).map {
        case (transporter, cargo) => new FerryPlan(transporter, cargo.cargo.toSet, to,
          mapLayers.rawWalkableMap.areaWhichContainsAsFree(to))
      }
    }, Nil)
    ferryPlans ++= newPlans
    trace(s"New plans: ${newPlans.mkString(", ")}")
    newPlans
  }

  def onTick(): Unit = {
    val done = ferryPlans.filterNot(_.unfinished)
    trace(s"Ferry plans done: $done")
    ferryPlans --= done
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

object PlanIdCounter {
  private var id = 0

  def nextId() = {
    id += 1
    id
  }

}

class FerryPlan(val ferry: TransporterUnit, initial: Set[GroundUnit], val toWhere: MapTilePosition,
                val targetArea: Option[Grid2D]) {
  private val planId = PlanIdCounter.nextId()

  def hasSpaceFor(forWhat: GroundUnit) = {
    takenSpace + forWhat.transportSize <= 8
  }

  def takenSpace = currentToTransport.iterator.map(_.transportSize).sum

  private var lastRequest        = ferry.currentTick
  private val currentToTransport = collection.mutable.HashSet.empty ++= initial

  def notifyRequested_!(): Unit = {
    lastRequest = ferry.currentTick
  }

  def withMore_!(gu: GroundUnit) = {
    trace(s"Adding $gu to plan $planId")
    currentToTransport += gu
    this
  }

  def orphaned = lastRequest + 48 < ferry.currentTick

  def toTransport: collection.Set[GroundUnit] = currentToTransport

  def unfinished = !orphaned || toTransport.exists { gu =>
    gu.currentArea != targetArea || gu.loaded
  } || loadedLeft

  def loadedLeft = {
    ferry.hasUnitsLoaded && ferry.currentArea == targetArea
  }

  def needsToReachTarget = ferry.currentArea != targetArea

  def covers(forWhat: GroundUnit) = {
    currentToTransport(forWhat)
  }

  def unloadedLeft = toTransport.exists { e =>
    e.onGround && e.currentArea != targetArea
  }

  assert(toTransport.map(_.transportSize).sum <= 8, s"Too many units for single transport: $toTransport")

}
