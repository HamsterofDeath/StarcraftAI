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

  /**
    * While the strategy seals the main and its wall stands, the main's terrain area is cut in two: what a worker can
    * walk to from the main command center with the standing buildings in the way (planned ones aside), and the largest
    * area beyond the wall. Both halves are one walkable area to the terrain, so the ferry checks ask this as well.
    * Pockets enclosed by buildings or minerals belong to neither side. Recomputed every tick: a breached wall seals
    * nothing.
    */
  private case class SealedSplit(mainArea: Grid2D, walkable: Grid2D, inside: Grid2D, outside: Grid2D) {

    /** Some(true) inside, Some(false) outside, None in a pocket; a tile under a building takes a free neighbour's. */
    def side(tile: MapTilePosition): Option[Boolean] = {
      def known(t: MapTilePosition) =
        if (!walkable.inBounds(t) || !walkable.free(t)) None
        else if (inside.free(t)) Some(true)
        else if (outside.free(t)) Some(false)
        else None
      known(tile).orElse(walkable.spiralAround(tile, 4).iterator.flatMap(known).nextOption())
    }
  }

  private var sealedSplit    = Option.empty[SealedSplit]
  private var lastSideReport = -1

  def sealing = sealedSplit.isDefined

  /** Whether the sealed wall stands between two tiles of the main's terrain area. */
  def sealedApart(a: MapTilePosition, b: MapTilePosition) = sealedSplit.exists { split =>
    split.mainArea.inBounds(a) && split.mainArea.inBounds(b) && split.mainArea.free(a) && split.mainArea.free(b) && {
      val (sa, sb) = (split.side(a), split.side(b))
      sa.isDefined && sb.isDefined && sa != sb
    }
  }

  private def updateSealedSplit(): Unit = {
    val before = sealedSplit.map(_.inside.freeCount)
    sealedSplit =
      if (!strategy.current.sealsMain || !universe.pluginByType[WallWithDepots].complete) None
      else {
        // built like the map layers' own walkable grids: the blocked sets first, then the terrain merged in
        val walkable = mapLayers.blockedByBuildingTiles.mutableCopy
          .or_!(mapLayers.blockedByResources.mutableCopy)
          .or_!(mapLayers.rawWalkableMap.mutableCopy)
          .guaranteeImmutability
        for {
          main     <- bases.mainBase
          anchor   <- walkable.nearestFree(main.mainBuilding.centerTile)
          inside   <- walkable.areaOf(anchor)
          mainArea <- mapLayers.rawWalkableMap.areaOf(anchor)
          // the wall must actually cut the main off: otherwise the inside reaches the rest of the terrain area
          if inside.freeCount * 2 < mainArea.freeCount
          outside <- walkable.areas.filterNot(_ == inside).maxByOpt(_.freeCount)
        } yield SealedSplit(mainArea, walkable, inside, outside)
      }
    sealedSplit.filter(_ => currentTick / 720 != lastSideReport).foreach { split =>
      lastSideReport = currentTick / 720
      val workers   = ownUnits.allByType[WorkerUnit].filter(w => w.isInGame && !w.isBeingCreated && w.onGround)
      val (in, out) = workers.partition(w => split.side(w.currentTile).contains(true))
      def idle(ws: Iterable[WorkerUnit]) = ws.count(w => unitManager.jobOf(w).isIdleOrDefault)
      NativeMatchEvidence.trace(
        "sealed-workers",
        s"inside=${in.size} idleInside=${idle(in)} outside=${out.size} idleOutside=${idle(out)} " +
          s"loaded=${ownUnits.allByType[WorkerUnit].count(_.loaded)} ferries=${ferryPlans.size} plans=" +
          ferryPlans.valuesIterator.map { p =>
            val to = scala.util.Try(p.toWhere).toOption
            s"${p.ferry.nativeUnitId}@${p.ferry.currentTile}->${to.getOrElse("?")}:reach=${p.needsToReachTarget}" +
              s":drop=${p.dropUnitsNow}:pick=${p.pickupTargetsLeft}:instant=${p.instantDropRequested}" +
              s":aboard=${p.ferry.loaded.size}:canDrop=${p.ferry.canDropHere}"
          }.mkString("|")
      )
    }
    val after = sealedSplit.map(_.inside.freeCount)
    if (before.isDefined != after.isDefined)
      NativeMatchEvidence.trace(
        if (after.isDefined) "main-sealed" else "main-unsealed",
        s"inside=${after.getOrElse(0)} area=${sealedSplit.map(_.mainArea.freeCount).getOrElse(0)}"
      )
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

      // a drop point moved to a free spot may lie on the unit's own side: then there is nothing to ferry
      def newPlan = {
        fixedDropTarget.filter { dropHere =>
          forWhat.currentArea != mapLayers.rawWalkableMap.areaOf(dropHere) ||
          sealedApart(forWhat.currentTile, dropHere)
        }.flatMap { dropHere =>
          newPlanFor(forWhat, dropHere, buildNewIfRequired).headOption
        }
      }
      ferryPlans.valuesIterator.find { plan =>
        lazy val sameTargetArea = {
          val area = fixedDropTarget.flatMap(mapLayers.rawWalkableMap.areaOf)
          plan.targetArea == area && plan.toWhere.distanceToIsLess(dropTarget, 15) &&
          !sealedApart(plan.toWhere, dropTarget)
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
      forWhat.currentArea != mapLayers.rawWalkableMap.areaOf(dropTarget) ||
        sealedApart(forWhat.currentTile, dropTarget),
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
    newPlans.foreach(p =>
      NativeMatchEvidence.trace(
        "ferry-plan",
        s"ferry=${p.ferry.nativeUnitId} cargo=${forWhat.nativeUnitId} from=${forWhat.currentTile} to=$dropTarget"
      )
    )
    newPlans
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    updateSealedSplit()
    val done = ferryPlans.valuesIterator.filterNot(_.unfinished)
    trace(s"Ferry plans done: $done")
    ferryPlans --= done.map(_.ferry)
    ferryPlans.valuesIterator.foreach(_.onTick_!())
    ifNth(Primes.prime67) {
      nearestFree.prepareNextIfDone()
    }
  }
}
