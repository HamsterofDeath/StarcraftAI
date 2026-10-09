package pony

import bwapi.Color
import pony.brain.{HasUniverse, Universe}

class BlockedBuildingSpotsRenderer(override val universe: Universe)
  extends AIPlugIn with HasUniverse {
  override val lazyWorld = universe.world

  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Orange)
        val area = mapLayers.blockedByBuildingTiles
        area.allBlocked
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.White)
        val area = mapLayers.blockedByPlannedBuildings
        area.allBlocked
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Blue)
        val area = mapLayers.blockedByResources
        area.allBlocked
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Grey)
        val area = mapLayers.blockedByWorkerPaths
        area.allBlocked
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }

      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Grey)
        val area = mapLayers.blockedByMobileUnits
        area.allBlocked
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }
    }
  }
}
