package pony

import bwapi.Color

class BuildableRenderer(ignoreWalkable: Boolean) extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Grey)

        val builable = lazyWorld.map.buildableGrid
        builable.allBlocked
        .filter { e => !ignoreWalkable || !lazyWorld.map.walkableGrid.blocked(e) }
        .foreach { blocked =>
          renderer.drawCrossedOutOnTile(blocked)
        }
      }
    }
  }
}
