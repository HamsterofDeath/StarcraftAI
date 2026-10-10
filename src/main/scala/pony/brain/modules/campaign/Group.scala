package pony
package brain
package modules
package campaign

import pony.geometry.{Grid2D, MapTilePosition}
import pony.units.{AllUnits, CanDie, WrapsUnit}

import scala.collection.mutable

class Group[T <: WrapsUnit](map: Grid2D, source: AllUnits) {
  def covers(u: CanDie) = myMembers.contains(u.nativeUnitId)

  private val maxDst    = 10 * 10
  private val myMembers = mutable.HashMap.empty[Int, MapTilePosition]
  private var myCenter  = MapTilePosition.zero

  def memberUnits = {
    val typed = survivingMembers
    assert(typed.size == size, s"Expected $size but found only ${typed.size}")
    typed
  }

  /** Queued observations may outlive their units; consumers of those queues must tolerate loss. */
  def survivingMembers: Vector[T] = memberIds.flatMap(source.byNativeId).toVector.asInstanceOf[Vector[T]]

  def size = myMembers.size

  def memberIds = myMembers.iterator.map(_._1)

  def center = myCenter

  def add_!(elem: (Int, MapTilePosition)): Unit = {
    myMembers += elem
    myCenter = evalCenter
  }

  private def evalCenter = {
    var x = 0
    var y = 0
    myMembers.foreach { case (_, p) =>
      x += p.x
      y += p.y
    }
    x /= myMembers.size
    y /= myMembers.size
    MapTilePosition.shared(x, y)
  }

  def canJoin(e: (Int, MapTilePosition)) = {
    e._2.distanceSquaredTo(myCenter) < maxDst && map.connectedByLine(myCenter, e._2)
  }
}
