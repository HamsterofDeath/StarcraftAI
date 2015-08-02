package pony

import bwapi.Player

import scala.collection.mutable.ArrayBuffer

trait AIAPI {
  private val plugins = ArrayBuffer.empty[AIPlugIn]

  def onReceiveText(player: Player, s: String): Unit = ???

  def onPlayerLeft(player: Player): Unit = ???

  def onPlayerDropped(player: Player): Unit = ???

  def onSendText(s: String): Unit = ???

  def onTick(): Unit = {
    try {
      world.tick()
      plugins.filter(_.isActive).foreach(_.onTick())
      world.postTick()
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

  def addPlugin(plugIn: AIPlugIn) = {
    plugins += plugIn
    plugIn.setWorld_!(world)
    this
  }

  def world:DefaultWorld

  def debugger = world.debugger
}



trait AIPlugIn  {
  private var active              = true
  private var myWorld: DefaultWorld = _

  def world = myWorld
  def render = world.debugger.render
  def debugger = world.debugger
  def orders = world.orderQueue

  def queueOrder(order: Order):Unit = {
    orders.queue_!(order)
  }

  def setWorld_!(world: DefaultWorld):Unit = {
    this.myWorld = world
  }

  def isActive = active

  def onTick():Unit = {
    if (active) {
      tickPlugIn()
    }
  }

  protected def tickPlugIn():Unit

  def off_!():Unit = active = false
  def on_!():Unit = active = true
}

trait AIPluginRunOnce extends AIPlugIn {
  def runOnce():Unit
  private var executed = false
  override protected def tickPlugIn(): Unit = {
    if (!executed) {
      executed = true
      runOnce()
    }
  }
}

