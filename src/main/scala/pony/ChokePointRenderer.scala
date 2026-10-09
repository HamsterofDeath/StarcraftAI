package pony

import bwapi.Color
import pony.brain.{HasUniverse, Universe}

class ChokePointRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)
      strategicMap.domains.foreach { case (choke, _) =>
        choke.lines.foreach { line =>
          renderer.drawLine(line.absoluteFrom, line.absoluteTo)
          renderer.drawTextAtTile(s"Chokepoint ${choke.index}", line.center)
        }
      }

      strategicMap.narrowPoints.foreach { narrow =>
        renderer.drawStar(narrow.where, 1)
        renderer.drawTextAtTile(s"Narrow ${narrow.index}", narrow.where)
      }
    }
  }
}
