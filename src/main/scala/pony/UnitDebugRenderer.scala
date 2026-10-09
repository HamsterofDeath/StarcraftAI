package pony

import bwapi.Color
import pony.brain.{HasUniverse, Universe}

class UnitDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      universe.ownUnits.allCompletedMobiles.filter(_.isSelected).foreach { m =>
        val center = m.currentPosition
        m match {
          case a: AirWeapon =>
            renderer.in_!(Color.Yellow).drawCircleAround(center, a.airRangePixels)
          case _ =>

        }
        m match {
          case g: GroundWeapon =>
            renderer.in_!(Color.Red).drawCircleAround(center, g.groundRangePixels)
          case _ =>

        }
      }

      universe.enemyUnits.allCompletedMobiles.filter(_.isHarmlessNow).foreach { cd =>
        renderer.in_!(Color.Green).drawCrossedOutOnTile(cd.currentTile)
      }
    }
  }
}
