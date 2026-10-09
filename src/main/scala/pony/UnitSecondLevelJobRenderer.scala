package pony

import pony.brain.{HasUniverse, Universe}

class UnitSecondLevelJobRenderer(override val universe: Universe)
  extends AIPlugIn with HasUniverse {

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>

    }
  }

  override def lazyWorld: DefaultWorld = universe.world
}
