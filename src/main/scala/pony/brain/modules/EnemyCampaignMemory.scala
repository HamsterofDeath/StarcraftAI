package pony
package brain
package modules

import scala.collection.mutable

/** Persistent knowledge consists only of native visible observations, never hidden enemy truth. */
class EnemyCampaignMemory {
  private val remembered = mutable.Map.empty[Int, ObservedEnemyBuilding]
  private var selected   = Option.empty[MapTilePosition]
  def buildings          = remembered.values.toVector.sortBy(_.id)
  def target             = selected
  def attackPosition     = selected.flatMap { site =>
    buildings.filter(_.tile.distanceToIsLess(site, 12))
      .sortBy(b => (!b.base, b.tile.distanceSquaredTo(site), b.tile.x, b.tile.y, b.id)).headOption.map(_.tile)
  }
  def update(
      visible: Seq[ObservedEnemyBuilding],
      observedDestroyed: Set[Int],
      visibleTile: MapTilePosition => Boolean
  ): Unit = {
    visible.foreach(b => remembered.put(b.id, b))
    val visibleIds = visible.map(_.id).toSet
    remembered.filterInPlace { (id, building) =>
      !observedDestroyed(id) && (visibleIds(id) || !building.footprint.forall(visibleTile))
    }
    selected = selected.filter(t => buildings.exists(_.tile.distanceToIsLess(t, 12)))
  }
  def select(from: MapTilePosition): Option[MapTilePosition] = {
    if (selected.isEmpty) {
      selected = buildings.sortBy(b => (!b.base, b.tile.distanceSquaredTo(from), b.tile.x, b.tile.y, b.id))
        .headOption.map(_.tile)
    }
    selected
  }
}
