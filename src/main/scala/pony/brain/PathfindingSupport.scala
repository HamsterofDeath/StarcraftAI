package pony
package brain

import pony.brain.jobs.JobOrSubJob
import pony.geometry.MapTilePosition
import pony.pathing.MigrationPath
import pony.render.Renderer
import pony.units.{AirUnit, GroundUnit, Mobile}
import pony.util.BWFuture

trait PathfindingSupport[T <: Mobile] extends JobOrSubJob[T] {

  private val needsPath = oncePer(Primes.prime23) {
    val target       = pathTargetPosition
    def areaOfTarget = target.flatMap(mapLayers.rawWalkableMap.areaOf)
    def areaOfUnit   = unit.currentArea

    target match {
      case Some(where) =>
        val far = unit.currentTile.distanceToIsMore(where, 15)
        far &&
        (unit match {
          // across a sealed wall the way leads by ferry, not on foot
          case g: GroundUnit
              if g.onGround && areaOfTarget == areaOfUnit &&
                areaOfTarget.isDefined && !ferryManager.sealedApart(unit.currentTile, where) =>
            !mapLayers.rawWalkableMap.connectedByLine(unit.currentTile, where)
          case a: AirUnit => true
          case _          => false
        })
      case None =>
        false
    }
  }
  private var myPath = BWFuture.none[MigrationPath]

  override def renderDebug(renderer: Renderer) = {
    super.renderDebug(renderer)
    myPath.ifDoneOpt(_.renderDebugPaths(renderer))

  }

  override def onTick_!() = {
    super.onTick_!()
    myPath.ifDoneOpt(_.onTick_!())
  }
  override def higherPriorityOrder = {
    def newPathRequired(where: MapTilePosition): Unit = {
      trace(s"Unit $unit needs paths to $where")
      val pf   = pathfinders.safeFor(unit)
      val task = pf.findPath(unit.currentTile, where).imap(_.toMigration(using universe))
      myPath = task
    }
    // must return noop instead of nil to cause a waiting behaviour
    def noopFallback: List[UnitOrder] = {
      if (waitForPath) {
        Orders.NoUpdate(unit).toList
      } else {
        Nil
      }
    }
    // the answer above is cached: a unit that boarded a ferry since has no path of its own to follow
    val aboard = unit match {
      case g: GroundUnit => !g.onGround
      case _             => false
    }
    val myOrder = {
      if (needsPath && !aboard) {
        pathTargetPosition.map { where =>
          if (myPath.isDone && myPath.assumeDoneAndGet.isEmpty) {
            newPathRequired(where)
          }

          myPath.foldOpt(noopFallback) { mig =>
            val outdated = mig.originalDestination.distanceToIsMore(where, 3)
            if (outdated) {
              newPathRequired(where)
              Nil
            } else {
              mig.nextPositionFor(unit)
                .map(Orders.MoveToTile(unit, _))
                .toList
            }
          }
        }.getOrElse(noopFallback)
      } else {
        myPath = BWFuture.none
        Nil
      }
    }
    if (myOrder.isEmpty) super.higherPriorityOrder else myOrder
  }

  protected def waitForPath: Boolean = true

  protected def pathTargetPosition: Option[MapTilePosition]
}
