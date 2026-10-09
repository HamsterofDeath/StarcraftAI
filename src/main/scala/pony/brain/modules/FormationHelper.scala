package pony
package brain
package modules

import scala.collection.mutable

class FormationHelper(override val universe: Universe,
                      paths: Paths,
                      distanceForUnits: Int = 1,
                      isGroundPath: Boolean) extends HasUniverse {

  private val target             = paths.realisticTarget
  private val assignments        = mutable.HashMap.empty[Mobile, MapTilePosition]
  private val used               = mutable.HashSet.empty[MapTilePosition]
  private val availablePositions = {
    val walkable = mapLayers.rawWalkableMap.guaranteeImmutability

    BWFuture.from {
      if (isGroundPath) {
        val availableArea = {
          mapLayers.safeGround.mutableCopy
        }

        val targetArea = walkable.areaOf(target).getOr(s"No area contains $target")
        val validTiles = availableArea.spiralAround(target)
                         .filter(availableArea.freeAndInBounds)
                         .filter(targetArea.freeAndInBounds)
                         .toVector

        val unsorted = validTiles.filter { p =>
          paths.isEmpty || paths.minimalDistanceTo(p) < 10
        }

        unsorted.sortBy(_.distanceSquaredTo(target))
      } else {
        val availableArea = {
          mapLayers.safeAir.mutableCopy
        }
        val validTiles = availableArea.spiralAround(target, 80)
                         .filter(availableArea.freeAndInBounds)
                         .toVector

        val unsorted = validTiles.filter { p => paths.isEmpty || paths.minimalDistanceTo(p) < 10 }

        unsorted.sortBy(_.distanceSquaredTo(target))
      }
    }
  }

  def formationTiles = assignments.valuesIterator

  def assignedPosition(mobile: Mobile) = {
    assignments.get(mobile).orElse {
      availablePositions.matchOnOptSelf(vec => {
        val hereOpt = vec.find(e => !used(e))
        hereOpt match {
          case Some(here) =>
            assignments.put(mobile, here)
            used += here

            hereOpt
          case _ =>
            trace(s"No open slot for $mobile")

            None
        }
      }, Option.empty)
    }
  }
}
