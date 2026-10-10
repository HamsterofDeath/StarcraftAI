package pony
package brain
package modules
package production

import pony.geometry.{Area, Grid2D}
import pony.render.Renderer
import pony.units.Mobile
import pony.util.FutureIterator

import bwapi.Color

import scala.reflect.ClassTag

class PreventBlockades(universe: Universe) extends DefaultBehaviour[Mobile](universe) {

  case class Feed(
      unitsToPositions: Map[Int, Area],
      baseArea: Grid2D,
      dangerous: Grid2D,
      blockedByMobiles: Grid2D
  )

  def feed = {
    val baseArea = {
      mapLayers.freeWalkableTiles.mutableCopy.guaranteeImmutability
    }
    val blockedByMobiles = mapLayers.blockedByMobileUnitsExtended
    val unitsToPositions = ownUnits.allCompletedMobiles
      .iterator
      .filter(_.canMove)
      .map { e => e.nativeUnitId -> e.blockedArea }
      .toMap
    val buildingLayer = mapLayers.freeWalkableTiles.guaranteeImmutability
    val dangerous     = mapLayers.slightlyDangerousForGroundAsBlocked.guaranteeImmutability
    Feed(unitsToPositions, baseArea, dangerous, blockedByMobiles)
  }

  private val unlockingPlan = FutureIterator.feed(feed).produceAsyncLater { in =>
    val operateOn = in.baseArea // .mutableCopy.or_!(in.blockedByMobiles.mutableCopy)

    val badlyPositioned = in.unitsToPositions.flatMap { case (id, where) =>
      val withOutline = where.growBy(tolerance)

      val evil = operateOn.cuttingAreas(withOutline)

      def safe = in.dangerous.free(where.centerTile)
      if (evil && safe) {
        Some(where.centerTile -> id)
      } else {
        None
      }
    }

    val unlockPositions = badlyPositioned.flatMap { case (tile, unitId) =>
      val moveTo = {
        val layer      = in.baseArea
        def candidates = layer.spiralAround(tile, 45)
          .drop(25)
          .sliding(1, 5)
          .flatten
          .filter(_.distanceToIsMore(tile, 3))
          .filter(layer.freeAndInBounds)

        candidates.find { e =>
          layer.freeAndInBounds(e.asArea.growBy(tolerance))
        }
      }
      moveTo.map { tile => unitId -> tile }
    }
    unlockPositions
  }.named("Make plan to unlock blockades")

  private val relevantLayer = oncePerTick {
    mapLayers.freeWalkableTiles.mutableCopy
      // .or_!(mapLayers.blockedByMobileUnitsExtended.mutableCopy)
      .guaranteeImmutability
  }

  override def forceRepeatedCommands: Boolean = false

  override def onTick_!() = {
    super.onTick_!()
    ifNth(Primes.prime59) {
      unlockingPlan.prepareNextIfDone()
    }
  }

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    unlockingPlan.onMostRecent { map =>
      map.foreach { case (unitId, whereTo) =>
        ownUnits.byId(unitId).foreach { unit =>
          renderer.in_!(Color.Red).indicateTarget(unit.centerTile, whereTo)
        }
      }
    }
  }

  override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {

    override def describeShort: String = "<->"

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      val layer = relevantLayer.get
      unlockingPlan.flatMap { plan =>
        plan.get(this.unit.nativeUnitId).map { Orders.MoveToTile(this.unit, _) }
      }.filter { command =>
        val near             = command.to.distanceToIsLess(this.unit.currentTile, 3)
        def stillProblematic = mapNth(Primes.prime37, true)(
          layer.cuttingAreas(this.unit.blockedArea.growBy(tolerance))
        )
        !near || stillProblematic
      }.map(_.toList)
        .getOrElse(Nil)
    }
  }

  private def tolerance = 1
}
