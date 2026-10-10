package pony
package render

import bwapi.Color

class WalkableRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    if (lazyWorld.debugger.isFullDebug) {
      lazyWorld.debugger.debugRender { renderer =>
        renderer.in_!(Color.Red)

        val walkable = lazyWorld.map.walkableGrid
        walkable.all
          .filter(walkable.blocked)
          .foreach { blocked =>
            renderer.drawCrossedOutOnTile(blocked)
          }
      }
    }
  }
}
