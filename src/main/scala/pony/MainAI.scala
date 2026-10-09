package pony

import pony.brain.{HasUniverse, TwilightSparkle}

class MainAI extends AIPlugIn with HasUniverse with AIAPIEventDispatcher {
  lazy val brain = new TwilightSparkle(lazyWorld)

  override def universe = brain.universe

  override protected def tickPlugIn(): Unit = {
    brain.queueOrdersForTick()
    if (debugger.isDebugging) {
      if (debugger.isRendering) {
        debugger.debugRender { ren =>
          brain.plugins.foreach(_.renderDebug(ren))
          brain.renderDebug(ren)
        }
      }
    }
  }

  override def debugger = world.debugger
}
