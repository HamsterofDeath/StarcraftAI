package pony
package render

import bwapi.Color
import pony.brain.{HasUniverse, Universe}

class PathDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Purple)
      universe.worldDominationPlan.allAttacks.foreach { attack =>
        val center = attack.currentCenter
        val count  = attack.force.size
        renderer.drawCircleAround(center.asMapPosition, math.round(math.sqrt(count)).toInt)
        attack.completePath.result.foreach { path =>
          path.renderDebug(renderer)
        }
      }
    }
  }
}
