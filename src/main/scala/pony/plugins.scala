package pony

import bwapi.Color
import pony.brain.{HasUniverse, TwilightSparkle, Universe}

abstract class ControllingAI extends AIPlugIn {

}

class MapReveal extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.chat("black sheep wall")
  }
}

class FastSpeed extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.faster()
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
        renderer.drawTextAtUnit(u, u.unitIdText)
      }
    }
  }
}

class UnitJobRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      unitManager.allJobs[Mobile].foreach { job =>
        renderer.drawTextAtUnit(job.unit, job.shortDebugString, 1)
      }
    }
  }
  override def world: DefaultWorld = universe.world
}

class StatsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val world = universe.world

  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.drawTextOnScreen("hello\nhello")
    }
  }
}

class BuildingSpotsRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val world = universe.world

  override protected def tickPlugIn(): Unit = {
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Orange)
      val area = mapLayers.blockedByBuildingTiles
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
      }
    }
    world.debugger.debugRender { renderer =>
      renderer.in_!(Color.Blue)
      val area = mapLayers.blockedByResources
      area.all.filter(area.blocked)
      .foreach { blocked =>
        renderer.drawCrossedOutOnTile(blocked)
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