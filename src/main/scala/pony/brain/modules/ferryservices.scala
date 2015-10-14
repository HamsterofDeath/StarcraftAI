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

  universe.register_!(() => {
    ferryPlans.foreach(_.afterTick_!())
  })

  def requestFerry(forWhat: GroundUnit, dropTarget: MapTilePosition, buildNewIfRequired: Boolean = false) = {
    val to = {
      dropTarget
      /*universe.mapLayers.reallyFreeBuildingTiles.nearestFreeBlock(dropTarget, 1)
      .getOr(s"Could not find free spot of size 3*3 around $dropTarget. Anywhere. At all.")*/
    }
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
    job.foreach(_.notifyRequested_!(forWhat))
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
  private val currentPlannedCargo = collection.mutable.HashMap.empty ++= initial.map(e => e -> ferry.currentTick)
  private val dropThese           = collection.mutable.HashSet.empty[GroundUnit]
  private val planId              = PlanIdCounter.nextId()

  def nextToDropAtTarget = {
    ferry.loaded.minByOpt(_.unitId)
  }

  def hasSpaceFor(forWhat: GroundUnit) = {
    takenSpace + forWhat.transportSize <= 8
  }

  def takenSpace = currentPlannedCargo.keysIterator.map(_.transportSize).sum

  def notifyRequested_!(gu: GroundUnit): Unit = {
    currentPlannedCargo.put(gu, ferry.currentTick)
  }

  def withMore_!(gu: GroundUnit) = {
    trace(s"Adding $gu to plan $planId")
    notifyRequested_!(gu)
    this
  }

  def toTransport = currentPlannedCargo.keySet

  def afterTick_!(): Unit = {
    val maxTick = currentPlannedCargo.valuesIterator.max

    val thoseChangedTheirMinds = currentPlannedCargo.iterator
                                 .filter(_._2 < maxTick)
                                 .map(_._1)


    dropThese ++= thoseChangedTheirMinds

    currentPlannedCargo --= dropThese

    val loadedButNotPlanned = ferry.loaded.filterNot(currentPlannedCargo.keySet)
    dropThese ++= loadedButNotPlanned

    dropThese.retain(ferry.isCarrying)
  }

  def toDropNow = {
    dropThese.headOption
  }

  def unfinished = toTransport.exists { gu =>
    gu.currentArea != targetArea || gu.loaded
  } || loadedLeft

  def loadedLeft = {
    ferry.hasUnitsLoaded && ferry.currentArea == targetArea
  }

  def needsToReachTarget = ferry.currentArea != targetArea

  def covers(forWhat: GroundUnit) = {
    currentPlannedCargo.contains(forWhat)
  }

  def unloadedLeft = toTransport.exists { e =>
    e.onGround && e.currentArea != targetArea
  }

  assert(toTransport.map(_.transportSize).sum <= 8, s"Too many units for single transport: $toTransport")

}
