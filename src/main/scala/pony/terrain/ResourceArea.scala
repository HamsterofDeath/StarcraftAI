package pony
package terrain

import pony.geometry.MapTilePosition
import pony.units.{Geysir, WrapsUnit}
import pony.util.LazyVal

import pony.brain.HasUniverse

case class ResourceArea(patches: Option[MineralPatchGroup], geysirs: Set[Geysir])
    extends HasUniverse {
  self =>
  private val id = WrapsUnit.nextId

  def uniqueId = id

  private val myArea = LazyVal.from {
    val ret = coveredTiles.flatMap(mapLayers.rawWalkableMap.areaOf)
    assert(ret.size == 1, s"Resources cover more than one area: $self")
    ret.head
  }

  def area = myArea.get

  def anyTile = coveredTiles.head

  assert(patches.isDefined || geysirs.nonEmpty)
  val resourceUnits                      = patches.map(_.patches).getOrElse(Nil) ++ geysirs
  val coveredTiles                       = resourceUnits.flatMap(_.area.tiles).toSet
  val center                             = patches.map(_.center).getOrElse(geysirs.head.tilePosition)
  private val myMostAnnoyingMinePosition = LazyVal.from {
    val blocked = mapLayers.rawWalkableMap.mutableCopy
      .or_!(mapLayers.blockedByResources.mutableCopy)
    blocked.nearestFreeBlock(center, 2).getOr(s"Could not detect free area near $self")
  }

  def nearbyFreeTile = myMostAnnoyingMinePosition.get

  def mineralsAndGas = resourceUnits.iterator.map(_.remaining).sum

  val allPatchTiles: Vector[MapTilePosition]  = patches.iterator.flatMap(_.allTiles).toVector
  val allGeysirTiles: Vector[MapTilePosition] = geysirs.flatMap(_.area.tiles).toVector

  def isPatchId(id: Int) = patches.fold(false)(_.patchId == id)

  override def universe = resourceUnits.head.universe

  def rich = {
    geysirs.iterator.map(_.remaining).sum > 1500 && patches.fold(0)(_.value) > 5000
  }
}
