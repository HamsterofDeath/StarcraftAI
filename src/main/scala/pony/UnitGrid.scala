package pony

import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable

class UnitGrid(override val universe: Universe) extends HasUniverse {
  val own   = new ViewOnGrid(this, false)
  val enemy = new ViewOnGrid(this, true)
  private val map        = universe.world.map
  private val myUnits    = Array.ofDim[mutable.HashSet[Mobile]](map.tileSizeX, map.tileSizeY)
  private val enemyUnits = Array.ofDim[mutable.HashSet[Mobile]](map.tileSizeX, map.tileSizeY)
  private val touched    = mutable.HashSet.empty[mutable.HashSet[Mobile]]

  def onTile(tile: MapTilePosition, hostile: Boolean) = {
    val set = on(hostile)(tile.x)(tile.y)
    if (set != null) set else Set.empty[Mobile]
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    //reset
    touched.foreach { modified =>
      modified.clear()
    }
    touched.clear()
    // update
    universe.ownUnits.allCompletedMobiles.foreach { m =>
      val units = {
        val pos = m.currentTile
        val existing = myUnits(pos.x)(pos.y)
        if (existing == null) {
          val newSet = mutable.HashSet.empty[Mobile]
          myUnits(pos.x)(pos.y) = newSet
          newSet
        } else {
          existing
        }
      }
      touched += units
      units += m
    }
    universe.enemyUnits.allCompletedMobiles.foreach { m =>
      val units = {
        val pos = m.currentTile
        val existing = enemyUnits(pos.x)(pos.y)
        if (existing == null) {
          val newSet = mutable.HashSet.empty[Mobile]
          enemyUnits(pos.x)(pos.y) = newSet
          newSet
        } else {
          existing
        }
      }
      touched += units
      units += m
    }

  }

  def allInRangeOf[T <: Mobile : Manifest](position: MapTilePosition, radius: Int,
                                           friendly: Boolean,
                                           customFilter: T => Boolean = (_: T) => true):
  Iterable[T] = {
    val onWhat = on(!friendly)

    geoHelper
    val fromX = 0 max position.x - radius
    val toX = map.tileSizeX min position.x + radius
    val fromY = 0 max position.y - radius
    val toY = map.tileSizeY min position.y + radius
    val radSqr = radius * radius
    val x2 = position.x
    val y2 = position.y
    def dstSqr(x: Int, y: Int) = {
      val xx = x - x2
      val yy = y - y2
      xx * xx + yy * yy
    }


    new Iterable[T] {
      override def iterator: Iterator[T] = {
        val filter = implicitly[Manifest[T]].runtimeClass
        (fromX until toX).iterator.flatMap { x =>
          (fromY until toY).iterator.filter(y => dstSqr(x, y) <= radSqr).flatMap { y =>
            val mobiles = onWhat(x)(y)
            if (mobiles == null) Iterator.empty
            else {
              mobiles.iterator
              .filter(filter.isInstance)
              .filter(e => customFilter(e.asInstanceOf[T]))
              .map(e => e.asInstanceOf[T])
            }
          }
        }
      }
    }
  }

  private def on(hostile: Boolean) = if (hostile) enemyUnits else myUnits
}
