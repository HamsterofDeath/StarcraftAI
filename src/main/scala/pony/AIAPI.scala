package pony

import scala.collection.mutable.ArrayBuffer

trait AIAPI {
  private val plugins = ArrayBuffer.empty[AIPlugIn]

  def onReceiveText(player: bwapi.Player, s: String): Unit = {
    trace(s"Received $s from $player")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onReceiveText(player, s) }
  }

  def onPlayerLeft(player: bwapi.Player): Unit = {
    trace(s"$player left")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onPlayerLeft(player) }
  }

  def onPlayerDropped(player: bwapi.Player): Unit = {
    trace(s"$player dropped")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onPlayerDropped(player) }
  }

  def onSendText(s: String): Unit = {
    trace(s"User send $s")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onSendText(s) }
  }

  private val aiMS     = ArrayBuffer.empty[Long]
  private val nativeMS = ArrayBuffer.empty[Long]

  def onTickOnApi(): Unit = {
    try {
      debugger.renderer.beforeTick()
      world.tick()
      val heavyFrame = AiCadence.heavyNow(world.tickCount)
      val before     = System.nanoTime()
      plugins.filter(_.isActive).foreach(_.onTickOnPlugin())
      val after   = System.nanoTime()
      val aiNanos = after - before
      aiMS += aiNanos
      world.postTick()
      val afterAfter  = System.nanoTime()
      val nativeNanos = afterAfter - after
      nativeMS += nativeNanos
      if (heavyFrame)
        NativeMatchEvidence.trace(
          "ai-heavy",
          s"pluginMs=${aiNanos / 1000000.0} nativeMs=${nativeNanos / 1000000.0}"
        )
      debug(
        s"AI took ${aiNanos.nanoToMillis} ms for calculations and then ${nativeNanos.nanoToMillis}"
      )
      if (aiMS.size > 100) aiMS.remove(0)
      if (nativeMS.size > 100) nativeMS.remove(0)
      val aiMillis     = (aiMS.sum.nanoToMillis / 100).format
      val nativeMillis = nativeMS.sum.nanoToMillis / 100
      debugger.renderer.drawTextOnScreen(s"AI: ${aiMillis}ms, Native ${nativeMillis.format}ms")
    } catch {
      case t: Throwable =>
        NativeMatchEvidence.failed(world.nativeGame, t)
        t.printStackTrace()
        System.exit(1)
    }
  }

  def addPlugin(plugIn: AIPlugIn) = {
    plugins += plugIn
    plugIn.setWorld_!(world)
    this
  }

  def world: DefaultWorld

  def debugger = world.debugger
}
