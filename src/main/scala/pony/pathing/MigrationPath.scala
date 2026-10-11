package pony
package pathing

import pony.brain.modules.campaign.FormationHelper
import pony.geometry.MapTilePosition
import pony.render.Renderer
import pony.units.Mobile

import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MigrationPath(follow: Paths, override val universe: Universe)
    extends HasUniverse {

  private val remaining       = mutable.HashMap.empty[Mobile, ArrayBuffer[MapTilePosition]]
  private val counter         = mutable.HashMap.empty[Mobile, Int]
  private val helper          = new FormationHelper(universe, follow, 2, isGroundPath)
  private val atFormationStep = mutable.HashSet.empty[Mobile]

  onTick_!()

  private val creationTick = universe.currentTick

  override def onTick_!() = {
    super.onTick_!()
    remaining.filterInPlace { case (unit, path) =>
      unit.isInGame && path.nonEmpty
    }
  }

  def isGroundPath = follow.isGroundPath

  def originalDestination = follow.unsafeTarget

  def safeDestination = follow.requestedTargetSafe

  def targetFormationTiles = helper.formationTiles

  def renderDebugPaths(renderer: Renderer): Unit = {
    follow.renderDebug(renderer)
  }

  def isCloseToUnsafeTarget(m: Mobile) = {
    (remaining.get(m) match {
      case None if counter.contains(m) => true
      case Some(path) if path.isEmpty  => true
      case _                           => false
    }) && m.currentTile.distanceToIsLess(originalDestination, 7)
  }

  def stillActiveUnits = counter.keysIterator.filter(_.isInGame)

  def meetingStats = {
    atFormationStep.size -> remaining.size
  }

  def allCloseToDestination = {
    val old = creationTick + 24 < universe.currentTick
    old && remaining.isEmpty && stillActiveUnits.nonEmpty
  }

  def nextPositionFor(t: Mobile) = nextFor(t).map(_._1)

  def nextFor(t: Mobile) = {
    assertCalled()
    val index                    = counter.getOrElseUpdate(t, counter.size) % follow.pathCount
    val initialFullPath          = follow.paths(index)
    val remainingPathForThisUnit = remaining.getOrElseUpdate(
      t,
      ArrayBuffer.empty ++= initialFullPath.waypoints
    )
    val closest              = remainingPathForThisUnit.minByOpt(_.distanceSquaredTo(t.currentTile))
    val formationPoint       = helper.assignedPosition(t)
    val canSeeFormationPoint = formationPoint.exists { target =>
      target.distanceToIsLess(t.currentTile, 15) &&
      mapLayers.rawWalkableMap.connectedByLine(target, t.currentTile)
    }
    def isOver30Percent = {
      remainingPathForThisUnit.size / initialFullPath.waypoints.size.toDouble <= 0.7
    }
    def manyArrived   = atFormationStep.size.toDouble / counter.size >= 0.8
    def skipFormation = {
      t.currentTile.distanceSquaredTo(initialFullPath.unsafeTarget) <
        formationPoint.map(t.currentTile.distanceSquaredTo).getOrElse(10000)
    }

    def clearPath() = remainingPathForThisUnit.clear()
    if (manyArrived) {
      clearPath()
      Some(follow.unsafeTarget -> true)
    } else if (skipFormation) {
      clearPath()
      atFormationStep += t
      None // just wait
    } else if (canSeeFormationPoint) {
      clearPath()
      atFormationStep += t
      formationPoint.map(e => e -> isOver30Percent)
    } else {
      closest.map { c =>
        if (c.distanceSquaredTo(t.currentTile) <= 25) {
          remainingPathForThisUnit.removeUntilInclusive(_ == c)
        }
        c -> isOver30Percent
      }
    }
  }
}
