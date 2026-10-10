package pony
package brain
package modules
package campaign

import pony.geometry.MapTilePosition
import pony.util.LazyVal

import scala.collection.mutable

class FormationAtFrontLineHelper(override val universe: Universe, distance: Int = 0)
    extends HasUniverse {
  private val myDefenseLines = LazyVal.from {
    bases.allBases.flatMap { base =>
      strategicMap.defenseLineOf(base).map(_.tileDistance(distance))
    }
  }

  private val blocked = mutable.HashMap.empty[MapTilePosition, BlacklistReason]

  def allOutsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator
      .flatMap { e =>
        e.pointsOutside
          .filterNot(blacklisted)
          .filter(map.free)
      }
  }

  def blacklisted(e: MapTilePosition) = blocked.contains(e)

  def defenseLines = myDefenseLines.get

  universe.bases.register(
    (base: Base) => {
      myDefenseLines.invalidate()
    },
    notifyForExisting = true
  )

  def allInsideNonBlacklisted = {
    val map = mapLayers.freeWalkableTiles
    defenseLines.iterator.flatMap(_.pointsInside).filterNot(blacklisted).filter(map.free)
  }

  def cleanBlacklist(dispose: (MapTilePosition, BlacklistReason) => Boolean) = {
    blocked.filter(e => dispose(e._1, e._2))
      .foreach(e => whiteList_!(e._1))
  }

  def whiteList_!(tilePosition: MapTilePosition): Unit = {
    blocked.remove(tilePosition)
  }

  def blackList_!(tile: MapTilePosition): Unit = {
    blocked.put(tile, BlacklistReason(universe.currentTick))
  }

  def reasonForBlacklisting(tilePosition: MapTilePosition) = blocked.get(tilePosition)

  case class BlacklistReason(when: Int)

}
