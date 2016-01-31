package pony
package brain.modules

import pony.brain._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class FerryManager(override val universe: Universe) extends HasUniverse {
  def nearestDropPointTo(where: MapTilePosition) = {
    nearestFree.flatMapOnContent(_.proposeFix(where))
  }

  private val ferryPlans = mutable.HashMap.empty[TransporterUnit, FerryPlan]

  private val employer         = new Employer[TransporterUnit](universe)

  case class Feed(walkableRaw: Grid2D, walkableNow: Grid2D, previous: AltDropPositions)

  private def feed = Feed(mapLayers.rawWalkableMap, mapLayers.freeWalkableTiles,
    Option(nearestFree).flatMap(_.mostRecent).getOrElse(AltDropPositions.empty))

  case class AltDropPositions(fixes: Map[MapTilePosition, MapTilePosition],
                              blockedImpossibles: Grid2D, passThrough: Grid2D) {
    def proposeFix(tile: MapTilePosition) = {
      if (passThrough.free(tile)) {
        tile.toSome
      } else if (blockedImpossibles.free(tile)) {
        fixes.get(tile)
      } else {
        None
      }
    }
  }

  object AltDropPositions {
    val empty = AltDropPositions(Map.empty, mapLayers.rawWalkableMap, mapLayers.rawWalkableMap)
  }

  private val nearestFree: FutureIterator[Feed, AltDropPositions] = FutureIterator.feed(feed)
                                                                    .produceAsyncLater { in =>
                                                                      val impossibleCache = in
                                                                                            .previous
                                                                                            .blockedImpossibles
                                                                                            .mutableCopy
                                                                      val passThrough = in
                                                                                        .walkableRaw
                                                                                        .mutableCopy

                                                                      val fixes = in.walkableRaw
                                                                                  .allFree
                                                                                  .flatMap { tile =>
                                                                                    val result = {
                                                                                      in.previous
                                                                                      .proposeFix(
                                                                                        tile)
                                                                                      .filter
                                                                                      { old =>
                                                                                        universe
                                                                                        .mapLayers
                                                                                        .rawWalkableMap
                                                                                        .freeAndInBounds(
                                                                                          old.asArea
                                                                                          .growBy(
                                                                                            2))
                                                                                      }
                                                                                      .orElse {
                                                                                        if (impossibleCache
                                                                                            .free(
                                                                                              tile)) {
                                                                                          val
                                                                                          maybePossible = in
                                                                                                              .walkableRaw
                                                                                                              .spiralAround(
                                                                                                                tile,
                                                                                                                10)
                                                                                                              .filter
                                                                                                              { alt =>
                                                                                                                val free = universe
                                                                                                                           .mapLayers
                                                                                                                           .rawWalkableMap
                                                                                                                           .freeAndInBounds(
                                                                                                                             alt
                                                                                                                             .asArea
                                                                                                                             .growBy(
                                                                                                                               2))
                                                                                                                def sameArea = universe
                                                                                                                               .mapLayers
                                                                                                                               .rawWalkableMap
                                                                                                                               .areInSameWalkableArea(
                                                                                                                                 alt,
                                                                                                                                 tile)
                                                                                                                free &&
                                                                                                                sameArea
                                                                                                              }
                                                                                                              .toSet

                                                                                          if (maybePossible
                                                                                              .isEmpty) {
                                                                                            impossibleCache
                                                                                            .block_!(
                                                                                              tile)
                                                                                            None
                                                                                          } else {
                                                                                            maybePossible
                                                                                            .find
                                                                                            { alt =>
                                                                                              universe
                                                                                              .mapLayers
                                                                                              .freeWalkableTiles
                                                                                              .freeAndInBounds(
                                                                                                alt)
                                                                                            }
                                                                                          }
                                                                                        } else {
                                                                                          None
                                                                                        }
                                                                                      }.map(
                                                                                        e => tile ->
                                                                                             e)
                                                                                    }

                                                                                    result.flatMap
                                                                                    { case tup@(from, to) =>
                                                                                      if (from ==
                                                                                          to) {
                                                                                        // covered by pass through
                                                                                        None
                                                                                      } else {
                                                                                        passThrough
                                                                                        .block_!(
                                                                                          from)
                                                                                        tup.toSome
                                                                                      }
                                                                                    }
                                                                                  }.toMap

                                                                      AltDropPositions(fixes,
                                                                        impossibleCache
                                                                        .guaranteeImmutability,
                                                                        passThrough
                                                                        .guaranteeImmutability)
                                                                    }

  def canDropHere(where: MapTilePosition) = {
    nearestDropPointTo(where).contains(where)
  }

  universe.register_!(() => {
    ferryPlans.valuesIterator.foreach(_.afterTick_!())
  })

  def planFor(ferry: TransporterUnit) = {
    ferryPlans.get(ferry)
  }

  def requestFerry_!(forWhat: GroundUnit, dropTarget: MapTilePosition,
                     buildNewIfRequired: Boolean = false) = {
    val job = {
      val fixedDropTarget = {
        val fixed = nearestFree.flatMapOnContent { data =>
          data.proposeFix(dropTarget)
        }
        trace(s"Requested ferry for $forWhat to go to $fixed")
        fixed
      }

      def newPlan = {
        fixedDropTarget.flatMap { dropHere =>
          newPlanFor(forWhat, dropHere, buildNewIfRequired).headOption
        }
      }
      ferryPlans.valuesIterator.find { plan =>
        lazy val sameArea = {
          val area = fixedDropTarget.flatMap(mapLayers.rawWalkableMap.areaWhichContainsAsFree)
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
      }.orElse(newPlan)
    }
    job
  }

  private def newPlanFor(forWhat: GroundUnit, dropTarget: MapTilePosition,
                         buildNewIfRequired: Boolean = false) = {
    assert(forWhat.currentArea != mapLayers.rawWalkableMap.areaWhichContainsAsFree(dropTarget),
      s"One of the units is already in the target area")

    assert(mapLayers.rawWalkableMap.free(dropTarget),
      s"$dropTarget is supposed to be a free ground tile")

    trace(s"Calculating new ferry job for $forWhat to $dropTarget")


    val selector = UnitJobRequest.idleOfType(employer, race.transporterClass, 1)
                   .withOnlyAccepting { ferry =>
                     !ferryPlans.contains(ferry)
                   }
    val result = unitManager.request(selector, buildNewIfRequired)
    val newPlans = result.ifNotZero(_.map { transporter =>
      new FerryPlan(transporter, forWhat, dropTarget,
        mapLayers.rawWalkableMap.areaWhichContainsAsFree(dropTarget))
    }, Nil)

    ferryPlans ++= newPlans.map(e => e.ferry -> e)
    trace(s"New plans: ${newPlans.mkString(", ")}")
    newPlans
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    val done = ferryPlans.valuesIterator.filterNot(_.unfinished)
    trace(s"Ferry plans done: $done")
    ferryPlans --= done.map(_.ferry)
    ferryPlans.valuesIterator.foreach(_.onTick_!())
    ifNth(Primes.prime67) {
      nearestFree.prepareNextIfDone()
    }
  }
}

class FerryCargoBuilder {
  private val myCargo = ArrayBuffer.empty[GroundUnit]
  private var left    = 8

  def cargo = myCargo.toSeq

  def add_!(g: GroundUnit) = {
    assert(canAdd(g))
    myCargo += g
    left -= g.transportSize
  }

  def canAdd(g: GroundUnit) = left >= g.transportSize
}

object PlanIdCounter {
  private var id = 0

  def nextId() = {
    id += 1
    id
  }

}

class FerryPlan(val ferry: TransporterUnit, initial: GroundUnit,
                initiallyPlannedToDropHere: MapTilePosition,
                val targetArea: Option[Grid2D]) extends HasUniverse {

  def toWhere = ferryManager.nearestDropPointTo(initiallyPlannedToDropHere)
                .getOr(s"No drop spot available anywhere near $initiallyPlannedToDropHere")

  private val currentPlannedCargo  = collection.mutable.HashMap.empty ++=
                                     initial.toSet.map(e => e -> ferry.currentTick)
  private val dropTheseImmediately = collection.mutable.HashSet.empty[GroundUnit]
  private val planId               = PlanIdCounter.nextId()
  private val myNextPickUp         = oncePerTick {
    toTransport.filterNot(_.loaded)
    .minByOpt(_.currentTile.distanceSquaredTo(ferry.currentTile))
  }
  private val myQueuedForPickup    = oncePerTick {
    toTransport.filter(needsToPickThatUp)
  }

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

  def withMore_!(gu: GroundUnit) = {
    trace(s"Adding $gu to plan $planId")
    notifyRequested_!(gu)
    this
  }

  def notifyRequested_!(gu: GroundUnit): Unit = {
    currentPlannedCargo.put(gu, ferry.currentTick)
    myNextPickUp.invalidate()
    myQueuedForPickup.invalidate()
    assert(takenSpace <= 8)
  }

  override def universe = ferry.universe

  def nextToPickUp = myNextPickUp.get

  def hasSpaceFor(forWhat: GroundUnit) = {
    takenSpace + forWhat.transportSize <= 8
  }

  def takenSpace = currentPlannedCargo.keysIterator.map(_.transportSize).sum

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

  assert(toTransport.map(_.transportSize).sum <= 8,
    s"Too many units for single transport: $toTransport")

}
