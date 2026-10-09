package pony
package brain.modules

import pony.brain._

class FerryPlan(
    val ferry: TransporterUnit,
    initial: GroundUnit,
    initiallyPlannedToDropHere: MapTilePosition,
    val targetArea: Option[Grid2D]
) extends HasUniverse {

  def toWhere = {
    ferryManager.nearestDropPointTo(initiallyPlannedToDropHere)
      .getOr(s"No drop spot available anywhere near $initiallyPlannedToDropHere")
  }

  private val currentPlannedCargo = collection.mutable.HashMap.empty ++=
    initial.toSet.map(_ -> ferry.currentTick)

  private val dropTheseImmediately = collection.mutable.HashSet.empty[GroundUnit]

  private val planId = PlanIdCounter.nextId()

  private def myNextPickUp = queuedForPickUp.headOption

  private val myQueuedForPickup = oncePerTick {
    toTransport.filter(needsToPickThatUp).toVector
      .sortBy(_.currentTile.distanceSquaredTo(ferry.currentTile))
  }

  private def shuttleCapacity = 8

  def replaceQueuedUnitIfPossible_!(maybeTransportThis: GroundUnit) = {
    val removeFromPlan = {
      queuedForPickUp.iterator
        .filter(takenSpace - _.transportSize + maybeTransportThis.transportSize <= shuttleCapacity)
        .filter { e =>
          e.currentTile.distanceSquaredTo(ferry.currentTile) - 25 >
            maybeTransportThis.currentTile.distanceSquaredTo(ferry.currentTile)
        }
        .maxByOpt(_.currentTile.distanceSquaredTo(ferry.currentTile))
    }

    removeFromPlan.foreach { old =>
      currentPlannedCargo.remove(old)
      withMore_!(maybeTransportThis)
    }
    removeFromPlan.isDefined
  }

  def withMore_!(gu: GroundUnit) = {
    trace(s"Adding $gu to plan $planId")
    notifyRequested_!(gu)
    this
  }

  def notifyRequested_!(gu: GroundUnit): Unit = {
    currentPlannedCargo.put(gu, ferry.currentTick)
    myQueuedForPickup.invalidate()
    assert(takenSpace <= shuttleCapacity)
  }

  override def universe = ferry.universe

  def nextToPickUp = myNextPickUp

  def hasSpaceFor(forWhat: GroundUnit) = {
    takenSpace + forWhat.transportSize <= shuttleCapacity
  }

  def takenSpace = currentPlannedCargo.keysIterator.map(_.transportSize).sum

  def afterTick_!(): Unit = {
    val maxTick = currentPlannedCargo.valuesIterator.maxOpt.getOrElse(Integer.MAX_VALUE)
    if (maxTick + 240 < currentTick) {
      warn("Investigation please")
      currentPlannedCargo.clear()
    }
    val thoseChangedTheirMinds = currentPlannedCargo
      .filter(_._2 + 12 < maxTick)
      .keySet

    dropTheseImmediately ++= thoseChangedTheirMinds

    currentPlannedCargo --= thoseChangedTheirMinds

    val loadedButNotPlanned = ferry.loaded.filterNot(currentPlannedCargo.keySet)
    dropTheseImmediately ++= loadedButNotPlanned

    dropTheseImmediately.filterInPlace { e =>
      ferry.isCarrying(e) && !currentPlannedCargo.contains(e)
    }
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

  def pickupTargetsLeft = queuedForPickUp.nonEmpty

  def queuedForPickUp = myQueuedForPickup.get

  def covers(forWhat: GroundUnit) = {
    currentPlannedCargo.contains(forWhat)
  }

  private def needsToPickThatUp(gu: GroundUnit) = {
    assert(toTransport(gu))
    gu.onGround && gu.currentArea != targetArea
  }

  def toTransport = currentPlannedCargo.keySet

  assert(
    toTransport.map(_.transportSize).sum <= shuttleCapacity,
    s"Too many units for single transport: $toTransport"
  )

}
