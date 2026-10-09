package pony
package brain.modules

import pony.brain._

import scala.collection.mutable

class FerryManager(override val universe: Universe) extends HasUniverse {
  def nearestDropPointTo(where: MapTilePosition) = {
    nearestFree.flatMapOnContent(_.proposeFix(where))
  }

  private val ferryPlans = mutable.HashMap.empty[TransporterUnit, FerryPlan]

  private val employer = new Employer[TransporterUnit](universe)

  case class Feed(walkableRaw: Grid2D, walkableNow: Grid2D, previous: AltDropPositions)

  private def feed = Feed(
    mapLayers.rawWalkableMap,
    mapLayers.freeWalkableTiles,
    Option(nearestFree).flatMap(_.mostRecent).getOrElse(AltDropPositions.empty)
  )

  case class AltDropPositions(
      fixes: Map[MapTilePosition, MapTilePosition],
      blockedImpossibles: Grid2D,
      passThrough: Grid2D
  ) {
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

  private val nearestFree: FutureIterator[Feed, AltDropPositions] = {
    FutureIterator.feed(feed)
      .produceAsyncLater { in =>
        val impossibleCache = in.previous.blockedImpossibles.mutableCopy
        val passThrough     = in.walkableRaw.mutableCopy

        val fixes = {
          in.walkableRaw
            .allFree
            .flatMap { tile =>
              val result = {
                in.previous.proposeFix(tile)
                  .filter { old =>
                    universe.mapLayers.rawWalkableMap
                      .freeAndInBounds(
                        old.asArea.growBy(2)
                      )
                  }
                  .orElse {
                    if (impossibleCache.free(tile)) {
                      val maybePossible = {
                        in.walkableRaw.spiralAround(tile, 10)
                          .filter { alt =>
                            val free = universe.mapLayers.rawWalkableMap
                              .freeAndInBounds(alt.asArea.growBy(2))
                            def sameArea = universe.mapLayers.rawWalkableMap
                              .areInSameWalkableArea(alt, tile)
                            free &&
                            sameArea
                          }.toSet
                      }

                      if (maybePossible.isEmpty) {
                        impossibleCache.block_!(tile)
                        None
                      } else {
                        maybePossible
                          .find { alt =>
                            universe.mapLayers.freeWalkableTiles.freeAndInBounds(alt)
                          }
                      }
                    } else {
                      None
                    }
                  }.map(e => tile -> e)
              }

              result.flatMap { case tup @ (from, to) =>
                if (from == to) {
                  // covered by pass through
                  None
                } else {
                  passThrough.block_!(from)
                  tup.toSome
                }
              }
            }.toMap
        }

        AltDropPositions(
          fixes,
          impossibleCache.guaranteeImmutability,
          passThrough.guaranteeImmutability
        )
      }.named("Best drop alternatives")
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

  def requestFerry_!(
      forWhat: GroundUnit,
      dropTarget: MapTilePosition,
      buildNewIfRequired: Boolean = false
  ) = {
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
        lazy val sameTargetArea = {
          val area = fixedDropTarget.flatMap(mapLayers.rawWalkableMap.areaOf)
          plan.targetArea == area && plan.toWhere.distanceToIsLess(dropTarget, 15)
        }
        def canAdd = {
          sameTargetArea && plan.hasSpaceFor(forWhat)
        }
        def tryReplace_!() = {
          sameTargetArea && plan.replaceQueuedUnitIfPossible_!(forWhat)
        }
        if (plan.covers(forWhat)) {
          true
        } else if (canAdd) {
          trace(s"Adding $forWhat to be transported by ${plan.ferry}")
          plan.withMore_!(forWhat)
          true
        } else {
          tryReplace_!()
        }
      }.orElse(newPlan)
    }
    job
  }

  private def newPlanFor(
      forWhat: GroundUnit,
      dropTarget: MapTilePosition,
      buildNewIfRequired: Boolean = false
  ) = {
    assert(
      forWhat.currentArea != mapLayers.rawWalkableMap.areaOf(dropTarget),
      s"One of the units is already in the target area"
    )

    assert(
      mapLayers.rawWalkableMap.free(dropTarget),
      s"$dropTarget is supposed to be a free ground tile"
    )

    trace(s"Calculating new ferry job for $forWhat to $dropTarget")

    val selector = UnitJobRequest.idleOfType(employer, race.transporterClass, 1)
      .withOnlyAccepting { ferry =>
        !ferryPlans.contains(ferry)
      }
    val result   = unitManager.request(selector, buildNewIfRequired)
    val newPlans = result.ifNotZero(
      _.map { transporter =>
        new FerryPlan(
          transporter,
          forWhat,
          dropTarget,
          mapLayers.rawWalkableMap.areaOf(dropTarget)
        )
      },
      Nil
    )

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
