package pony
package brain
package modules

import scala.collection.mutable.ArrayBuffer

object GroupingHelper {
  def typedGroup[T <: WrapsUnit](universe: Universe, group: Group[T]) = {
    val members = group.memberIds.flatMap { e =>
      universe.enemyUnits
      .byId(e)
      .orElse(universe.ownUnits.byId(e))
      .asInstanceOf[Option[T]]
    }.toVector
    UnitGroup(members, group.center)
  }

  def groupThese[T <: WrapsUnit](seq: TraversableOnce[T], universe: Universe) = {
    val helper = new GroupingHelper(universe.mapLayers.rawWalkableMap.guaranteeImmutability, seq,
      universe.allUnits)
    BWFuture(Option(helper.evaluateUnitGroups))
  }

  def groupTheseNow[T <: WrapsUnit](seq: TraversableOnce[T], universe: Universe) = {
    val helper = new GroupingHelper(universe.mapLayers.rawWalkableMap.guaranteeImmutability, seq,
      universe.allUnits)
    helper.evaluateUnitGroups
  }

  def groupTheseNow[T <: WrapsUnit](seq: TraversableOnce[T], map: Grid2D, allUnits: AllUnits) = {
    val helper = new GroupingHelper(map.guaranteeImmutability, seq, allUnits)
    helper.evaluateUnitGroups
  }
}

class GroupingHelper[T <: WrapsUnit](val map: Grid2D, seq: TraversableOnce[T], source: AllUnits) {
  private val immutable: Vector[(Int, MapTilePosition)] = seq.iterator.map { u => (u.nativeUnitId, u.centerTile) }.toVector

  /**
    * can/should be run asynchronously
    *
    * @return
    */
  def evaluateUnitGroups: Seq[Group[T]] = {
    val groups = ArrayBuffer.empty[Group[T]]
    immutable.foreach { elem =>
      groups.find(_.canJoin(elem)) match {
        case Some(joinMe) => joinMe.add_!(elem)
        case None =>
          val ng = new Group[T](map, source)
          groups += ng
          ng.add_!(elem)
      }
    }
    groups.immutableView
  }

}
