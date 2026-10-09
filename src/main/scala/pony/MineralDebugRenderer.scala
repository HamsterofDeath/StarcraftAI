package pony

import bwapi.Color
import pony.brain.modules.GatherMineralsAtSinglePatch
import pony.brain.{HasUniverse, Universe}

class MineralDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Yellow)
      world.resourceAnalyzer.groups.foreach { mpg =>
        mpg.patches.foreach { mp =>
          renderer.writeText(mp.tilePosition, s"#${mpg.patchId}")
        }
      }

      universe.unitManager.allJobsByType[GatherMineralsAtSinglePatch].groupBy(_.targetPatch)
      .foreach { case (k, v) =>
        val estimatedWorkerCount = v.head.requiredWorkers
        renderer.drawTextAtStaticUnit(v.head.targetPatch, estimatedWorkerCount.toString, 1)
      }

    }
  }
}
