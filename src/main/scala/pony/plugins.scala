package pony

import bwapi.Color
import pony.brain.TwilightSparkle

abstract class ControllingAI extends AIPlugIn {

}

class MapReveal extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.chat("black sheep wall")
  }
}

class WalkableRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Red)

      world.map.walkableGrid.all
      .filter(world.map.walkableGrid.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
  }
}

class UnitIdRenderer extends AIPlugIn {
  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Green)

      world.units.mineByType[Mobile].foreach { u =>
        renderer.drawTextAboveUnit(u, u.unitIdText)
      }
    }
  }
}

class MainAI extends AIPlugIn {
  lazy val brain = new TwilightSparkle(world)

  override protected def tickPlugIn(): Unit = {
    brain.queueOrdersForTick()
  }
}