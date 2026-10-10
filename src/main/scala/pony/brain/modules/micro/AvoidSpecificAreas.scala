package pony
package brain
package modules
package micro

import pony.combat.Weapon
import pony.geometry.{Grid2D, MapTilePosition}
import pony.render.Renderer
import pony.units.Mobile
import pony.util.FutureIterator

import bwapi.Color

import scala.collection.mutable
import scala.reflect.ClassTag

abstract class AvoidSpecificAreas[T <: Mobile: ClassTag](universe: Universe)
    extends DefaultBehaviour[T](universe) {
  self =>

  protected def tilesToAvoidAsBlocked: Option[Grid2D]

  protected def tolerance = 2

  case class Feed(toAvoid: Grid2D, free: Grid2D)

  protected def targetReuseAllowance = 0

  private def feed = Feed(
    tilesToAvoidAsBlocked.getOrElse(mapLayers.emptyGrid),
    mapLayers.freeWalkableTiles
  )

  private val freeAlternativeTiles = FutureIterator.feed(feed).produceAsyncLater { in =>
    val usages     = mutable.HashMap.empty[MapTilePosition, Int]
    val walkable   = mapLayers.rawWalkableMap
    val allBlocked = in.toAvoid.allBlocked.toList
    val mut        = in.toAvoid.mutableCopy
    allBlocked.foreach { where =>
      mut.block_!(where.asArea.growBy(tolerance))
    }

    mut.allBlocked.toVector.flatMap { blocked =>
      val ret = in.free.spiralAround(blocked).find { tile =>
        in.free.freeAndInBounds(tile) &&
        mut.freeAndInBounds(tile) &&
        walkable.areInSameWalkableArea(tile, blocked) &&
        walkable.countBlockedOnLine(tile, blocked).freePercentage >= 0.8
      }
      // block solution for next try
      ret.foreach { found =>
        usages.insertReplace(found, _ + 1, 1)
        if (usages(found) == targetReuseAllowance) {
          mut.block_!(found)
        }
      }
      ret.map(e => blocked -> e)
    }.toMap
  }.named("Find alternative tiles")

  protected def debugColor = Color.Green

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    freeAlternativeTiles.foreach { data =>
      data.foreach { case (from, to) =>
        renderer.in_!(debugColor).drawCircleAround(to)
      }
    }
  }

  protected def updateWhen = Primes.prime31

  override def onTick_!() = {
    super.onTick_!()
    ifNth(updateWhen) {
      freeAlternativeTiles.prepareNextIfDone()
    }
  }

  protected val actionName = self.getClass.className

  override protected def wrapBase(unit: T) = new SingleUnitBehaviour[T](unit, meta) {

    private val isInstantFireUnit = this.unit.isInstantFireUnit

    private def weapon = this.unit.asInstanceOf[Weapon]

    override protected def butOnlyIf = {
      def couldFireInBetween = {
        isInstantFireUnit &&
        weapon.isReadyToFireWeapon &&
        weapon.hasTarget
      }
      super.butOnlyIf && !couldFireInBetween
    }

    override def describeShort = actionName

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      freeAlternativeTiles.flatMapOnContent { result =>
        result.get(this.unit.currentTile)
      }.map { target =>
        Orders.MoveToTile(this.unit, target)
      }.toList
    }
  }
}
