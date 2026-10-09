package pony

import scala.compiletime.uninitialized

trait AIPlugIn {
  private var active                = true
  private var myWorld: DefaultWorld = uninitialized

  def debugger                           = lazyWorld.debugger
  def queueOrder(order: UnitOrder): Unit = {
    orders.queue_!(order)
  }
  def orders                                = lazyWorld.orderQueue
  def lazyWorld                             = myWorld
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
  def on_!(): Unit  = active = true
  protected def tickPlugIn(): Unit
}
