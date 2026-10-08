package pony
package brain
package modules

import scala.collection.mutable

/** The sky-wall opening seals the main base's land approach with Supply Depots. */
class WallWithDepots(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe)
  with BuildingRequestHelper {

  private var anchors = Option.empty[Vector[MapTilePosition]]
  private var reportedNone = false
  private var reportedComplete = false
  private val lastAttempt = mutable.Map.empty[MapTilePosition, Int]

  private def active = race.isTerran && (strategy.current match {
    case _: Strategy.TerranSkyWall => true
    case _ => false
  })

  private def depotFree(anchor: MapTilePosition): Boolean = {
    val area = Area(anchor, Size(2, 2))
    mapLayers.rawWalkableMap.insideBounds(anchor) && area.tiles.forall { t =>
      mapLayers.rawWalkableMap.free(t) &&
      mapLayers.freeTilesForConstruction.free(t) &&
      mapLayers.blockedByBuildingTiles.free(t) &&
      mapLayers.blockedByPlannedBuildings.free(t) &&
      mapLayers.blockedByPotentialAddons.free(t)
    }
  }

  /** Corridor tiles of the main choke, each replaced by a depot footprint that can host it. */
  private def computeWall(home: Base): Vector[MapTilePosition] = {
    val corridor = mutable.Set.empty[MapTilePosition]
    strategicMap.defenseLineOf(home).toVector.foreach { front =>
      front.chokePoint.lines.foreach { cutting =>
        AreaHelper.traverseTilesOfLine(cutting.absoluteFrom, cutting.absoluteTo,
          (x, y) => corridor += MapTilePosition(x, y))
      }
    }
    val spots = corridor.toVector
      .filter(mapLayers.rawWalkableMap.insideBounds)
      .filter(mapLayers.rawWalkableMap.free)
      .flatMap { t =>
        Vector(t, MapTilePosition(t.x - 1, t.y), MapTilePosition(t.x, t.y - 1), MapTilePosition(t.x - 1, t.y - 1))
          .find(depotFree)
      }.distinct.sortBy(p => (p.y, p.x))
    // A line too wide for a few depots is not a wall; refuse instead of fencing the map.
    if (spots.size <= 8) spots else Vector.empty
  }

  override def onTick_!(): Unit = {
    if (!active || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    bases.mainBase.foreach { home =>
      val wall = anchors.getOrElse {
        val chosen = computeWall(home)
        if (chosen.nonEmpty) {
          anchors = Some(chosen)
          NativeMatchEvidence.trace("wall-planned", s"depots=${chosen.size} at=${chosen.mkString(",")}")
        }
        chosen
      }
      if (wall.isEmpty) {
        if (!reportedNone) {
          NativeMatchEvidence.trace("wall-none", "no achievable land choke; skipping the depot wall")
          reportedNone = true
        }
      } else {
        val existing = ownUnits.allByType[SupplyDepot].filter(_.isInGame).map(_.tilePosition).toSet
        val pending = (unitManager.requestedConstructions[SupplyDepot].flatMap(_.customPosition.requestedPosition) ++
          unitManager.constructionsInProgress[SupplyDepot].map(_.buildWhere)).toSet
        val missing = wall.filterNot(a => existing(a) || pending(a))
        if (missing.isEmpty) {
          if (!reportedComplete) {
            NativeMatchEvidence.trace("wall-complete", s"depots=${wall.size}")
            reportedComplete = true
          }
        } else {
          missing.filter(depotFree).foreach { a =>
            val due = currentTick - lastAttempt.getOrElse(a, Int.MinValue) > 24 * 60
            if (due && !pending(a) && !existing(a)) {
              lastAttempt(a) = currentTick
              NativeMatchEvidence.trace("wall-depot-request", s"at=$a")
              requestBuilding(classOf[SupplyDepot], takeCareOfDependencies = false,
                customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(a)(depotFree(a)),
                priority = Priority.Expand)
            }
          }
        }
      }
    }
  }
}
