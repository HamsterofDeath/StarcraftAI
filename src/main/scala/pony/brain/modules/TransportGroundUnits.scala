package pony
package brain
package modules

import bwapi.Color

import scala.collection.mutable
import scala.reflect.ClassTag

class TransportGroundUnits(universe: Universe)
    extends DefaultBehaviour[TransporterUnit](universe) {

  private val timeBetweenUpdates = 24
  private val maxAge             = 24 * 4

  override def forceRepeatedCommands: Boolean = false

  override protected def wrapBase(unit: TransporterUnit) = new SingleUnitBehaviour[TransporterUnit](unit, meta) {

    override def renderDebug(r: Renderer) = {
      super.renderDebug(r)
      ferryManager.planFor(this.unit).foreach { plan =>
        val describe = {
          val fly         = if (plan.needsToReachTarget) "fly, " else ""
          val unload      = if (plan.dropUnitsNow) "unload, " else ""
          val fetch       = if (plan.pickupTargetsLeft) "fetch, " else ""
          val instantDrop = if (plan.instantDropRequested) "drop, " else ""
          s"$fly$unload$fetch$instantDrop"
        }

        if (plan.needsToReachTarget) {
          r.in_!(Color.Green)
          r.indicateTarget(this.unit.currentTile, plan.toWhere)
        }
        if (plan.pickupTargetsLeft) {
          r.in_!(Color.Orange)
          r.indicateTarget(this.unit.currentTile, plan.nextToPickUp.map(_.currentTile).get)
        }

        if (plan.dropUnitsNow) {
          r.in_!(Color.Red)
          r.indicateTarget(this.unit.currentTile, plan.nextToDrop.map(_.currentTile).get)
        }

        r.drawTextAtMobileUnit(this.unit, describe, 2)
      }
    }

    trait PositionOrUnit {
      def where: MapTilePosition
      def unit: Option[GroundUnit]
    }

    case class Feed(
        transporterWhere: MapTilePosition,
        pickupTarget: PositionOrUnit,
        pathfinder: PathFinder
    )

    case class IsPosition(where: MapTilePosition) extends PositionOrUnit {
      override def unit = None
    }

    case class IsUnit(basedOn: GroundUnit) extends PositionOrUnit {
      override val where = basedOn.currentTile

      private val u = basedOn.toSome

      override def unit = u
    }

    case class MaybePath(path: FutureIterator[Feed, Option[MigrationPath]]) {

      def onTick_!() = {
        path.onMostRecent(_.foreach(_.onTick_!()))
      }

      private var lastTouched = currentTick

      def age = currentTick - lastTouched

      def updateFuture(): Unit = {
        trace(s"Update requested for path to ${path.feedObj}")
        path.prepareNextIfDone()
        lastTouched = currentTick
      }
    }

    private val paths = mutable.HashMap.empty[PositionOrUnit, MaybePath]

    override def forceRepeats: Boolean = true

    override def blocksForTicks: Int = 24

    override def describeShort: String = "Transport"

    override def onTick_!() = {
      super.onTick_!()
      paths.valuesIterator.foreach(_.onTick_!())
    }

    override def toOrder(what: Objective): Seq[UnitOrder] = {
      val old = {
        paths.filter {
          case (_, maybe) => maybe.age > maxAge
        }
          .keySet
          .toList
      }
      paths --= old
      trace(s"Kicked out obsolete paths for: $old", old.nonEmpty)

      def currentSafeOrder(transporterTarget: PositionOrUnit): Option[UnitOrder] = {
        val maybeCalculatedPath = paths.getOrElseUpdate(
          transporterTarget, {
            def feed = Feed(this.unit.currentTile, transporterTarget, pathfinders.airSafe)

            val future = FutureIterator.feed(feed).produceAsync { in =>
              in.pathfinder.findPathNow(in.transporterWhere, in.pickupTarget.where)
                .map(_.toMigration(using this.universe))
            }.named("Pathfinding")
            MaybePath(future)
          }
        )
        if (maybeCalculatedPath.age > timeBetweenUpdates) {
          maybeCalculatedPath.updateFuture()
        }
        maybeCalculatedPath.path.flatMapOnContent { maybeFoundPath =>
          maybeFoundPath.map { safePath =>
            def simpleCommand = {
              transporterTarget.unit match {
                case Some(pickupTarget) =>
                  Orders.LoadUnit(this.unit, pickupTarget)
                case None =>
                  Orders.MoveToTile(this.unit, transporterTarget.where)
              }
            }
            val near = this.unit.currentTile.distanceToIsLess(transporterTarget.where, 5)
            if (near)
              simpleCommand
            else
              safePath.nextPositionFor(this.unit).map { where =>
                Orders.MoveToTile(this.unit, where)
              }.getOrElse(simpleCommand)
          }
        }
      }

      def orderByUnit(groundUnit: GroundUnit): Option[UnitOrder] = {
        val what = IsUnit(groundUnit)
        currentSafeOrder(what)
      }

      def orderByTile(simpleTile: MapTilePosition): Option[UnitOrder] = {
        val what = IsPosition(simpleTile)
        currentSafeOrder(what)
      }

      val order = {
        ferryManager.planFor(this.unit) match {
          case Some(plan) =>
            if ((plan.instantDropRequested || plan.dropUnitsNow) && !this.unit.canDropHere) {
              // a ferry hovering with no order keeps its cargo for good: fly straight there until a path is known
              this.unit.nearestDropTile.orElse(Some(plan.toWhere)).flatMap { tile =>
                orderByTile(tile).orElse(Some(Orders.MoveToTile(this.unit, tile)))
              }
            } else if (plan.instantDropRequested && this.unit.canDropHere) {
              plan.asapDrop.map { dropIt =>
                Orders.UnloadUnit(this.unit, dropIt)
              }
            } else if (plan.dropUnitsNow) {
              plan.nextToDrop.map { drop =>
                Orders.UnloadUnit(this.unit, drop)
              }
            } else if (plan.pickupTargetsLeft) {
              val loadThis = plan.nextToPickUp
              orderByUnit(loadThis.get)
            } else if (plan.needsToReachTarget) {
              orderByTile(plan.toWhere)
            } else {
              None
            }
          case None =>
            if (this.unit.hasUnitsLoaded) {
              val nearestFree = mapLayers.freeWalkableTiles.nearestFree(this.unit.currentTile)
              nearestFree.map { where =>
                Orders.UnloadAll(this.unit, where).forceRepeat_!(true)
              }
            } else if (this.unit.isPickingUp) {
              Orders.Stop(this.unit).toSome
            } else {
              None
            }
        }
      }
      order.toList
    }
  }
}
