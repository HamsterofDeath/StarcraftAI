package pony

import scala.collection.mutable

case class MineralPatchGroup(patchId: Int) {

  def anyTile = allTiles.next()

  private val myPatches      = mutable.HashSet.empty[MineralPatch]
  private val myCenter       = new LazyVal[MapTilePosition](calcCenter)
  private val myValue        = new
      LazyVal[Int](myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))
  private val myInitialValue = LazyVal.from(myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))

  def allTiles = myPatches.iterator.flatMap(_.area.tiles)

  def tick() = {
    myValue.invalidate()
  }

  def addPatch(mp: MineralPatch): Unit = {
    myPatches += mp
    myCenter.invalidate()
    myInitialValue.invalidate()
  }

  def remainingPercentage = myValue / myInitialValue.toDouble

  override def toString = s"Minerals#$patchId($value)@$center"

  def center = myCenter.get

  def value = myValue.get

  def initialValue = myInitialValue.get

  def patches = myPatches.toSet

  def contains(mp: MineralPatch): Boolean = myPatches(mp)

  private def calcCenter = {
    val (x, y) = myPatches.foldLeft((0, 0)) {
      case ((x, y), mp) => (x + mp.tilePosition.x, y + mp.tilePosition.y)
    }
    val reference = MapTilePosition.shared(x / myPatches.size, y / myPatches.size)
    myPatches.flatMap(_.area.tiles).minBy(_.distanceSquaredTo(reference))
  }
}
