package pony

import bwapi.Player

import scala.collection.mutable.ArrayBuffer

trait AIAPI {
  private val plugins = ArrayBuffer.empty[AIPlugIn]

  def onReceiveText(player: Player, s: String): Unit = {
    trace(s"Received $s from $player")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onReceiveText(player, s) }
  }

  def onPlayerLeft(player: Player): Unit = {
    trace(s"$player left")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onPlayerLeft(player) }
  }

  def onPlayerDropped(player: Player): Unit = {
    trace(s"$player dropped")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onPlayerDropped(player) }
  }

  def onSendText(s: String): Unit = {
    trace(s"User send $s")
    plugins.collect { case receiver: AIAPIEventDispatcher => receiver.onSendText(s)
    }
  }

  private val aiMS     = ArrayBuffer.empty[Long]
  private val nativeMS = ArrayBuffer.empty[Long]

  def onTickOnApi(): Unit = {
    try {
      debugger.renderer.beforeTick()
      world.tick()
      val before = System.nanoTime()
      plugins.filter(_.isActive).foreach(_.onTickOnPlugin())
      val after = System.nanoTime()
      val aiNanos = after - before
      aiMS += aiNanos
      world.postTick()
      val afterAfter = System.nanoTime()
      val nativeNanos = afterAfter - after
      nativeMS += nativeNanos
      debug(
        s"AI took ${aiNanos.nanoToMillis} ms for calculations and then ${nativeNanos.nanoToMillis}")
      if (aiMS.size > 100) aiMS.remove(0)
      if (nativeMS.size > 100) nativeMS.remove(0)
      val aiMillis = (aiMS.sum.nanoToMillis / 100).format
      val nativeMillis = nativeMS.sum.nanoToMillis / 100
      debugger.renderer.drawTextOnScreen(s"AI: ${aiMillis}ms, Native ${nativeMillis.format}ms")
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
        System.exit(0)
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

trait AIAPIEventDispatcher extends AIAPI {
  private val receivers = ArrayBuffer.empty[AIAPI]

  def listen_!(aiApi: AIAPI): Unit = {
    receivers += aiApi
  }

  override def onReceiveText(player: Player, s: String): Unit = {
    super.onReceiveText(player, s)
    receivers.foreach(_.onReceiveText(player, s))
  }

  override def onPlayerLeft(player: Player): Unit = {
    super.onPlayerLeft(player)
    receivers.foreach(_.onPlayerLeft(player))
  }

  override def onPlayerDropped(player: Player): Unit = {
    super.onPlayerDropped(player)
    receivers.foreach(_.onPlayerDropped(player))
  }

  override def onSendText(s: String): Unit = {
    super.onSendText(s)
    receivers.foreach(_.onSendText(s))
  }
}

trait AIPlugIn {
  private var active                = true
  private var myWorld: DefaultWorld = _

  def debugger = lazyWorld.debugger
  def queueOrder(order: UnitOrder): Unit = {
    orders.queue_!(order)
  }
  def orders = lazyWorld.orderQueue
  def lazyWorld = myWorld
  def setWorld_!(world: DefaultWorld): Unit = {
    this.myWorld = world
  }

  def isActive = active

  def onTickOnPlugin(): Unit = {
    if (active) {
      tickPlugIn()
    }
  }
  def off_!(): Unit = active = false
  def on_!(): Unit = active = true
  protected def tickPlugIn(): Unit
}

trait AIPluginRunOnce extends AIPlugIn {
  private var executed = false
  def runOnce(): Unit
  override protected def tickPlugIn(): Unit = {
    if (!executed) {
      executed = true
      runOnce()
    }
  }
}

