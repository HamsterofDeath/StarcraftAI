package pony
package brain.modules

import pony.brain._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FerryManager(override val universe: Universe) extends HasUniverse {

  private val ferryPlans = mutable.HashMap.empty[TransporterUnit, FerryPlan]

  private val employer = new Employer[TransporterUnit](universe)

  def planFor(ferry: TransporterUnit) = {
    ferryPlans.get(ferry)
  }

  universe.register_!(() => {
    ferryPlans.valuesIterator.foreach(_.afterTick_!())
  })

  private val nearestFreeCache = oncePer(Primes.prime67) {
    mutable.HashMap.empty[MapTilePosition, MapTilePosition]
  }

  def requestFerry(forWhat: GroundUnit, dropTarget: MapTilePosition,
                   buildNewIfRequired: Boolean = false) = {
    val job = {
      lazy val to = {
        val fixed = nearestFreeCache.get.getOrElseUpdate(dropTarget, {
          universe.mapLayers.freeWalkableTiles.nearestFreeBlock(dropTarget, 1).getOr(
            s"Could not find free spot of size 3*3 around $dropTarget. Anywhere. At all.")
        })
        trace(s"Requested ferry for $forWhat to go to $fixed")
        fixed
      }

      ferryPlans.valuesIterator.find { plan =>
        lazy val sameArea = {
          val area = mapLayers.rawWalkableMap.areaWhichContainsAsFree(to)
          plan.targetArea == area
        }
        def canAdd = {
          sameArea && plan.hasSpaceFor(forWhat)
        }
        def tryReplace_!() = {
          sameArea && plan.replaceQueuedUnitIfPossible_!(forWhat)
        }
        if (plan.covers(forWhat)) {
          true
        } else if (canAdd) {
          plan.withMore_!(forWhat)
          true
        } else {
          tryReplace_!()
        }
      }.orElse(newPlanFor(forWhat, to, buildNewIfRequired).headOption)
    }
    job
  }

  private def newPlanFor(forWhat: GroundUnit, to: MapTilePosition,
                         buildNewIfRequired: Boolean = false) = {
    assert(forWhat.currentArea != mapLayers.rawWalkableMap.areaWhichContainsAsFree(to),
      s"One of the units is already in the target area")

    assert(mapLayers.rawWalkableMap.free(to), s"$to is supposed to be a free ground tile")

    trace(s"Calculating new ferry job for $forWhat to $to")


    val selector = UnitJobRequest.idleOfType(employer, race.transporterClass, 1)
                   .acceptOnly_! { ferry =>
                     !ferryPlans.contains(ferry)
                   }
    val result = unitManager.request(selector, buildNewIfRequired)
    val newPlans = result.ifNotZero(_.map { transporter =>
      new FerryPlan(transporter, forWhat, to,
        mapLayers.rawWalkableMap.areaWhichContainsAsFree(to))
    }, Nil)

    ferryPlans ++= newPlans.map(e => e.ferry -> e)
    trace(s"New plans: ${newPlans.mkString(", ")}")
    newPlans
  }

  override def onTick(): Unit = {
    super.onTick()
    val done = ferryPlans.valuesIterator.filterNot(_.unfinished)
    trace(s"Ferry plans done: $done")
    ferryPlans --= done.map(_.ferry)
    ferryPlans.valuesIterator.foreach(_.onTick())
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

class FerryPlan(val ferry: TransporterUnit, initial: GroundUnit, val toWhere: MapTilePosition,
                val targetArea: Option[Grid2D]) extends HasUniverse {
  def replaceQueuedUnitIfPossible_!(maybeTransportThis: GroundUnit) = {
    val removeFromPlan = queuedForPickUp.iterator
                         .filter(_.transportSize >= maybeTransportThis.transportSize)
                         .minByOpt(_.currentTile.distanceSquaredTo(ferry.currentTile))
    removeFromPlan.foreach { old =>
      currentPlannedCargo.remove(old)
      withMore_!(maybeTransportThis)
    }
    removeFromPlan.isDefined
  }

  def queuedForPickUp = myQueuedForPickup.get

  private val currentPlannedCargo = collection.mutable.HashMap.empty ++=
                                    initial.toSet.map(e => e -> ferry.currentTick)

  private val dropTheseImmediately = collection.mutable.HashSet.empty[GroundUnit]
  private val planId               = PlanIdCounter.nextId()

  override def universe = ferry.universe

  private val myNextPickUp = oncePerTick {
    toTransport.filterNot(_.loaded)
    .minByOpt(_.currentTile.distanceSquaredTo(ferry.currentTile))
  }

  def nextToPickUp = myNextPickUp.get

  def hasSpaceFor(forWhat: GroundUnit) = {
    takenSpace + forWhat.transportSize <= 8
  }

  def takenSpace = currentPlannedCargo.keysIterator.map(_.transportSize).sum

  def notifyRequested_!(gu: GroundUnit): Unit = {
    currentPlannedCargo.put(gu, ferry.currentTick)
    myNextPickUp.invalidate()
    myQueuedForPickup.invalidate()
    assert(takenSpace <= 8)
  }

  def withMore_!(gu: GroundUnit) = {
    trace(s"Adding $gu to plan $planId")
    notifyRequested_!(gu)
    this
  }

  def toTransport = currentPlannedCargo.keySet

  def afterTick_!(): Unit = {
    val maxTick = currentPlannedCargo.valuesIterator.maxOpt.getOrElse(Integer.MAX_VALUE)

    val thoseChangedTheirMinds = currentPlannedCargo
                                 .filter(_._2 + 12 < maxTick)
                                 .keySet


    dropTheseImmediately ++= thoseChangedTheirMinds

    currentPlannedCargo --= dropTheseImmediately

    val loadedButNotPlanned = ferry.loaded.filterNot(currentPlannedCargo.keySet)
    dropTheseImmediately ++= loadedButNotPlanned

    dropTheseImmediately.retain(e => ferry.isCarrying(e) && thoseChangedTheirMinds(e))
  }

  def nextToDrop = {
    ferry.loaded.headOption
  }

  def instantDropRequested = asapDrop.isDefined

  def asapDrop = {
    dropTheseImmediately.headOption
  }

  def unfinished = {
    needsToReachTarget || pickupTargetsLeft || dropUnitsNow
  }

  def dropUnitsNow = {
    ferry.hasUnitsLoaded && ferry.currentArea == targetArea
  }

  def needsToReachTarget = ferry.currentArea != targetArea

  def covers(forWhat: GroundUnit) = {
    currentPlannedCargo.contains(forWhat)
  }

  private def needsToPickThatUp(gu: GroundUnit) = {
    assert(toTransport(gu))
    gu.onGround && gu.currentArea != targetArea
  }
  private val myQueuedForPickup = oncePerTick {
    toTransport.filter(needsToPickThatUp)
  }

  def pickupTargetsLeft = queuedForPickUp.nonEmpty

  assert(toTransport.map(_.transportSize).sum <= 8,
    s"Too many units for single transport: $toTransport")

}
